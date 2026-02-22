package tree_sitter_mollie_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_mollie "github.com/tree-sitter/tree-sitter-mollie/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_mollie.Language())
	if language == nil {
		t.Errorf("Error loading Mollie grammar")
	}
}
