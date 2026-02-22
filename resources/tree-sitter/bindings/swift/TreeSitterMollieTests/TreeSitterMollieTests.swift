import XCTest
import SwiftTreeSitter
import TreeSitterMollie

final class TreeSitterMollieTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_mollie())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Mollie grammar")
    }
}
