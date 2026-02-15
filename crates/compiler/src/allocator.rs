use std::{
    alloc,
    collections::HashSet,
    hash::{BuildHasherDefault, DefaultHasher},
    mem,
    sync::Mutex,
};

use cranelift::codegen::ir;
use itertools::Itertools;
use mollie_index::Idx;
use mollie_ir::{MollieType, Struct};
use mollie_typing::{AdtKind, AdtVariantRef};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub enum TypeLayoutField {
    Regular,
    Collectable,
    ArrayOfRegular,
    ArrayOfFat,
}

#[derive(Debug, PartialEq, Eq)]
#[repr(C)]
pub struct TypeLayout {
    pub fields: &'static [(AdtVariantRef, u32, MollieType, TypeLayoutField)],
    pub adt_ty: Option<u64>,
    pub size: usize,
    pub align: usize,
    pub kind: Option<AdtKind>,
}

impl TypeLayout {
    pub const fn of<T>() -> Self {
        let size = mem::size_of::<T>();
        let align = mem::align_of::<T>();

        Self {
            size,
            align,
            fields: &[],
            kind: None,
            adt_ty: None,
        }
    }

    pub fn static_of<T>() -> &'static Self {
        let size = mem::size_of::<T>();
        let align = mem::align_of::<T>();

        Box::leak(Box::new(Self {
            size,
            align,
            fields: &[],
            kind: None,
            adt_ty: None,
        }))
    }
}

bitflags::bitflags! {
    #[derive(Debug)]
    pub struct GcValueInfo: usize {
        const MARKED = 1;
        const ARRAY = 1 << 1;
    }
}

#[derive(Debug)]
#[repr(C)]
pub struct GcValue<T> {
    pub layout: &'static TypeLayout,
    // although `marked` can only be `0` or `1`, for correct alignment of `value` pointers, we need it to be `usize`
    pub info: GcValueInfo,
    pub value: T,
}

pub struct GarbageCollector {
    pub objects: HashSet<*mut GcValue<()>, BuildHasherDefault<DefaultHasher>>,
    pub roots: HashSet<*mut GcValue<()>, BuildHasherDefault<DefaultHasher>>,
    pub allocated_bytes: usize,
    pub deallocated_bytes: usize,
    pub perform_gc_at: usize,
}

impl GarbageCollector {
    /// # Safety
    ///
    /// Provided [`GcValue`] pointer should point to valid data.
    #[track_caller]
    pub unsafe fn mark(ptr: *mut GcValue<()>) {
        let root = unsafe { &mut *ptr };

        root.info.insert(GcValueInfo::MARKED);

        let value_ptr = unsafe { ptr.byte_add(mem::offset_of!(GcValue<()>, value)) };
        let current_variant = if matches!(root.layout.kind, Some(AdtKind::Enum)) {
            unsafe { value_ptr.cast::<AdtVariantRef>().read() }
        } else {
            AdtVariantRef::ZERO
        };

        for &(variant, offset, _, ty) in root.layout.fields {
            if variant == current_variant {
                println!("Marking {variant:?}:{offset}:{ty:?} of {root:?}");

                match ty {
                    TypeLayoutField::Regular => (),
                    TypeLayoutField::Collectable => {
                        let node = unsafe {
                            value_ptr
                                .byte_add(offset as usize)
                                .cast::<*mut ()>()
                                .read()
                                .byte_sub(mem::offset_of!(GcValue<()>, value))
                        }
                        .cast::<GcValue<()>>();

                        unsafe { Self::mark(node) };
                    }
                    TypeLayoutField::ArrayOfRegular => {
                        let elements = unsafe { value_ptr.byte_add(offset as usize).cast::<&[*mut GcValue<()>]>().read() };

                        for &value_ptr in elements {
                            let node = unsafe { value_ptr.byte_sub(mem::offset_of!(GcValue<()>, value)) };

                            unsafe { Self::mark(node) };
                        }
                    }
                    TypeLayoutField::ArrayOfFat => {
                        let elements = unsafe { value_ptr.byte_add(offset as usize).cast::<&[(*mut GcValue<()>, usize)]>().read() };

                        for &(value_ptr, _) in elements {
                            let node = unsafe { value_ptr.byte_sub(mem::offset_of!(GcValue<()>, value)) };

                            unsafe { Self::mark(node) };
                        }
                    }
                }
            }
        }
    }

    /// # Safety
    ///
    /// All objects in [`GarbageCollector::objects`] should point to valid data.
    pub unsafe fn sweep(&mut self) {
        self.objects.retain(|object_ptr| {
            let object = unsafe { &mut **object_ptr };

            if object.info.contains(GcValueInfo::MARKED) {
                object.info.remove(GcValueInfo::MARKED);

                true
            } else {
                let size = object.layout.size + const { mem::size_of::<GcValue<()>>() };
                let align = object.layout.align.max(const { mem::align_of::<GcValue<()>>() });

                let layout = unsafe { alloc::Layout::from_size_align_unchecked(size, align) };

                unsafe { alloc::dealloc(object_ptr.cast(), layout) };

                self.allocated_bytes -= layout.size();
                self.deallocated_bytes += layout.size();

                false
            }
        });
    }

    /// # Safety
    ///
    /// All objects in [`GarbageCollector::roots`] should point to valid data.
    pub unsafe fn collect(&mut self) {
        for &root in &self.roots {
            println!("Marking root");

            unsafe { Self::mark(root) };
        }

        unsafe { self.sweep() };
    }

    /// # Safety
    ///
    /// Provided [`TypeLayout`] should be valid.
    pub unsafe fn alloc(&mut self, type_layout: &'static TypeLayout) -> *mut () {
        let max_variant = type_layout.fields.iter().max_by_key(|f| f.0.index()).map(|f| f.0).unwrap_or_default();
        let max_variant = if max_variant.index() > 0 {
            let variants = type_layout.fields.iter().fold(vec![Vec::new(); max_variant.index() + 1], |mut prev, cur| {
                prev[cur.0.index()].push(cur);

                prev
            });

            variants
                .into_iter()
                .map(|variant| (variant[0].0, Struct::new(variant.iter().map(|f| (f.2, None)))))
                .max_by_key(|s| s.1.size)
                .map(|s| s.0)
                .unwrap_or_default()
        } else {
            max_variant
        };

        let Struct { size, align, .. } = Struct::new(
            [
                (MollieType::Regular(ir::Type::int_with_byte_size(size_of::<usize>() as u16).unwrap()), None),
                (MollieType::Regular(ir::Type::int_with_byte_size(size_of::<usize>() as u16).unwrap()), None),
            ]
            .into_iter()
            .chain(type_layout.fields.iter().filter(|f| f.0 == max_variant).map(|field| (field.2, None))),
        );

        let layout = unsafe { alloc::Layout::from_size_align(size as usize, align as usize).unwrap() };
        let ptr: *mut GcValue<()> = unsafe { alloc::alloc_zeroed(layout) }.cast();

        unsafe {
            let value = ptr.as_mut().unwrap_unchecked();

            value.layout = type_layout;
            value.info = GcValueInfo::empty();
        }

        self.objects.insert(ptr);
        self.allocated_bytes += layout.size();

        if self.allocated_bytes >= self.perform_gc_at {
            unsafe { self.collect() };
        }

        unsafe { ptr.byte_add(mem::offset_of!(GcValue<()>, value)).cast() }
    }

    /// # Safety
    ///
    /// Provided [`TypeLayout`] should be valid.
    pub unsafe fn alloc_array(&mut self, item_layout: &'static TypeLayout, length: usize) -> *mut () {
        let capacity = compute_capacity(item_layout.size, length);
        let layout = alloc::Layout::new::<GcValue<Array>>();

        let ptr = unsafe { alloc::alloc_zeroed(layout) }.cast();
        let array_layout = alloc::Layout::from_size_align(item_layout.size * capacity, item_layout.align).unwrap();
        let array_ptr = unsafe { alloc::alloc_zeroed(array_layout) }.cast();

        self.objects.insert(ptr);

        unsafe {
            let value = ptr.cast::<GcValue<Array>>().as_mut().unwrap_unchecked();

            value.layout = item_layout;
            value.info = GcValueInfo::ARRAY;
            value.value = Array {
                length,
                capacity,
                ptr: array_ptr,
            }
        }

        self.allocated_bytes += layout.size();
        self.allocated_bytes += array_layout.size();

        if self.allocated_bytes >= self.perform_gc_at {
            unsafe { self.collect() };
        }

        unsafe { ptr.byte_add(mem::offset_of!(GcValue<()>, value)).cast::<()>() }
    }

    /// # Safety
    ///
    /// Provided [`TypeLayout`] should be valid.
    pub unsafe fn realloc_array(&mut self, value: *mut GcValue<Array>, length: usize) -> *mut () {
        let array = unsafe { value.byte_sub(mem::offset_of!(GcValue<()>, value)) };
        let item_layout = (unsafe { &*array }).layout;

        let old_capacity = unsafe { (&*array).value.capacity };
        let old_layout = unsafe { alloc::Layout::from_size_align_unchecked(item_layout.size * old_capacity, item_layout.align) };

        let new_capacity = compute_capacity(item_layout.size, length);
        let new_layout = unsafe { alloc::Layout::from_size_align_unchecked(item_layout.size * new_capacity, item_layout.align) };

        let array_ptr = unsafe { alloc::realloc((&*array).value.ptr.cast(), old_layout, new_layout.size()) }.cast();

        unsafe {
            let value = array.as_mut().unwrap_unchecked();

            value.value = Array {
                length,
                capacity: new_capacity,
                ptr: array_ptr,
            };
        }

        self.allocated_bytes += new_layout.size();
        self.allocated_bytes -= old_layout.size();

        if self.allocated_bytes >= self.perform_gc_at {
            unsafe { self.collect() };
        }

        value.cast()
    }
}

#[derive(Debug)]
#[repr(C)]
pub struct Array {
    pub length: usize,
    pub capacity: usize,
    pub ptr: *mut (),
}

fn compute_capacity(size: usize, required: usize) -> usize {
    required.next_power_of_two().max(match size {
        1 => 8,
        size if size <= 1024 => 4,
        _ => 1,
    })
}

unsafe impl Send for GarbageCollector {}

pub static GARBAGE_COLLECTOR: Mutex<GarbageCollector> = Mutex::new(GarbageCollector {
    objects: HashSet::with_hasher(BuildHasherDefault::new()),
    roots: HashSet::with_hasher(BuildHasherDefault::new()),
    allocated_bytes: 0,
    deallocated_bytes: 0,
    perform_gc_at: 1024 * 1024,
});

/// # Safety
///
/// Provided [`TypeLayout`] should be valid.
pub unsafe fn alloc(type_layout: &'static TypeLayout) -> *mut () {
    let mut allocator = unsafe { GARBAGE_COLLECTOR.lock().unwrap_unchecked() };

    unsafe { allocator.alloc(type_layout) }
}

/// # Safety
///
/// Provided [`TypeLayout`] should be valid.
pub unsafe fn alloc_array(item_layout: &'static TypeLayout, length: usize) -> *mut () {
    let mut allocator = unsafe { GARBAGE_COLLECTOR.lock().unwrap_unchecked() };

    unsafe { allocator.alloc_array(item_layout, length) }
}

/// # Safety
///
/// Provided [`TypeLayout`] should be valid.
pub unsafe fn realloc_array(array_ptr: *mut GcValue<Array>, length: usize) -> *mut () {
    let mut allocator = unsafe { GARBAGE_COLLECTOR.lock().unwrap_unchecked() };

    unsafe { allocator.realloc_array(array_ptr, length) }
}

/// # Safety
///
/// Provided [`GcValue`] pointer should be point to valid data.
pub unsafe fn mark_root(value_ptr: *mut GcValue<()>) {
    let value = unsafe { value_ptr.byte_sub(mem::offset_of!(GcValue<()>, value)) };
    let mut allocator = unsafe { GARBAGE_COLLECTOR.lock().unwrap_unchecked() };

    allocator.roots.insert(value);
}

/// # Safety
///
/// Provided [`GcValue`] pointer should be point to valid data.
pub unsafe fn unmark_root(value_ptr: *mut GcValue<()>) {
    let value = unsafe { value_ptr.byte_sub(mem::offset_of!(GcValue<()>, value)) };
    let mut allocator = unsafe { GARBAGE_COLLECTOR.lock().unwrap_unchecked() };

    allocator.roots.remove(&value);
}
