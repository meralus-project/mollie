use std::{
    alloc,
    collections::HashSet,
    hash::{BuildHasherDefault, DefaultHasher},
    mem,
    sync::Mutex,
};

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub enum GcManagedFieldType {
    Regular,
    ArrayOfRegular,
    ArrayOfFat,
}

#[derive(Debug)]
#[repr(C)]
pub struct TypeLayout {
    pub size: usize,
    pub align: usize,
    pub gc_managed_fields: &'static [(u32, GcManagedFieldType)],
}

impl TypeLayout {
    pub const fn of<T>() -> Self {
        let size = mem::size_of::<T>();
        let align = mem::align_of::<T>();

        Self {
            size,
            align,
            gc_managed_fields: &[],
        }
    }

    pub fn as_layout(&self) -> Result<alloc::Layout, alloc::LayoutError> {
        alloc::Layout::from_size_align(self.size, self.align)
    }
}

#[repr(C)]
pub struct GcValue<T> {
    pub layout: &'static TypeLayout,
    pub marked: bool,
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
    pub unsafe fn mark(ptr: *mut GcValue<()>) {
        let root = unsafe { &mut *ptr };

        root.marked = true;

        let value_ptr = unsafe { ptr.byte_add(mem::offset_of!(GcValue<()>, value)) };

        for &(offset, ty) in root.layout.gc_managed_fields {
            match ty {
                GcManagedFieldType::Regular => {
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
                GcManagedFieldType::ArrayOfRegular => {
                    let elements = unsafe { value_ptr.byte_add(offset as usize).cast::<&[*mut GcValue<()>]>().read() };

                    for &value_ptr in elements {
                        let node = unsafe { value_ptr.byte_sub(mem::offset_of!(GcValue<()>, value)) };

                        unsafe { Self::mark(node) };
                    }
                }
                GcManagedFieldType::ArrayOfFat => {
                    let elements = unsafe { value_ptr.byte_add(offset as usize).cast::<&[(*mut GcValue<()>, usize)]>().read() };

                    for &(value_ptr, _) in elements {
                        let node = unsafe { value_ptr.byte_sub(mem::offset_of!(GcValue<()>, value)) };

                        unsafe { Self::mark(node) };
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

            if object.marked {
                object.marked = false;

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
            unsafe { Self::mark(root) };
        }

        unsafe { self.sweep() };
    }

    /// # Safety
    ///
    /// Provided [`TypeLayout`] should be valid.
    pub unsafe fn alloc(&mut self, type_layout: &'static TypeLayout) -> *const () {
        let size = type_layout.size + const { mem::size_of::<GcValue<()>>() };
        let align = type_layout.align.max(const { mem::align_of::<GcValue<()>>() });

        let layout = unsafe { alloc::Layout::from_size_align_unchecked(size, align) };
        let ptr: *mut GcValue<()> = unsafe { alloc::alloc_zeroed(layout) }.cast();

        unsafe {
            let value = ptr.as_mut().unwrap_unchecked();

            value.layout = type_layout;
            value.marked = false;
        }

        self.objects.insert(ptr);
        self.allocated_bytes += layout.size();

        if self.allocated_bytes >= self.perform_gc_at {
            unsafe { self.collect() };
        }

        unsafe { ptr.byte_add(mem::offset_of!(GcValue<()>, value)).cast() }
    }
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
pub unsafe fn alloc(type_layout: &'static TypeLayout) -> *const () {
    let mut allocator = unsafe { GARBAGE_COLLECTOR.lock().unwrap_unchecked() };

    unsafe { allocator.alloc(type_layout) }
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
