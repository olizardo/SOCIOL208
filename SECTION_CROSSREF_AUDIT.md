# Section & Document Cross-Reference Audit
**Generated:** 2026-06-08

## Executive Summary

Your .qmd files have **55 additional broken cross-references** beyond the equation/figure references already cleaned:

| Type | Found | Broken | Status |
|------|-------|--------|--------|
| **Section refs** (`@sec-`) | 0 | 0 | ✓ None |
| **Table refs** (`@tbl-`) | 9 | 9 | ⚠ All broken |
| **File links** (`[text](file.qmd)`) | 60 | 46 | ⚠ 77% broken |

---

## 1. Section Cross-References (`@sec-*`)

**Status:** ✓ **No issues**

- **Defined:** 0
- **Referenced:** 0
- **Broken:** 0

You don't currently use structured section references in your documents.

---

## 2. Table Cross-References (`@tbl-*`)

**Status:** ⚠ **9 broken references**

### What's wrong:
- Only **1 table is defined** with a label: `{##tbl-status}` (note: double `##`)
- **9 tables are referenced** but have no matching definitions

### Broken references:

| Reference | Referenced In | Definition Status |
|-----------|--------------|------------------|
| `@tbl-fclc` | tm-fitness.qmd | ✗ Missing |
| `@tbl-groups` | prest-tm-gen.qmd | ✗ Missing |
| `@tbl-ha` | prest-hits.qmd | ✗ Missing |
| `@tbl-people` | prest-tm-gen.qmd | ✗ Missing |
| `@tbl-perm-1` | stat-qap.qmd | ✗ Missing |
| `@tbl-perm-2` | stat-qap.qmd | ✗ Missing |
| `@tbl-ref` | tm-reflections.qmd | ✗ Missing |
| `@tbl-status` | (defined but in wrong format: `##tbl-status`) | ✗ Wrong format |
| `@tbl-sw` | prest-tm-gen.qmd | ✗ Missing |

### Recommendation:
- **Option A:** Remove all 9 `@tbl-*` references from text (like we did with eq/fig)
- **Option B:** Add missing table labels to your R/Python code chunks that generate tables

---

## 3. Markdown File Links

**Status:** ⚠ **46 broken links** (out of 60 total)

These are markdown-style links to `.qmd` files that don't exist:

```
[text](missing-file.qmd)
```

### Missing files referenced:

| Missing File | Referenced In | Count |
|--------------|--------------|-------|
| `tm-prest-eigen.qmd` | prest-tm-gen.qmd, tm-ca.qmd, tm-fitness.qmd, tm-reflections.qmd | 9 |
| `sim-se.qmd` | sim-gen.qmd, sim-loc.qmd | 4 |
| `tm-basic.qmd` | cent-tm.qmd, stat-tm.qmd | 4 |
| `eigen.qmd` | comm-spectral.qmd, prest-hits.qmd, prest-tm.qmd, tm-ca.qmd | 4 |
| `swap.qmd` | stat-ergm1.qmd, stat-qap.qmd, stat-swap.qmd, stat-tm.qmd | 4 |
| `spectral.qmd` | comm-tm-spectral.qmd | 2 |
| `qap.qmd` | stat-ergm1.qmd, stat-swap.qmd | 2 |
| `degcent.qmd` | prest-eigen.qmd, prest-hits.qmd | 2 |
| `globsim.qmd` | prest-eigen.qmd | 2 |
| `tm-duality.qmd` | sim-tm.qmd, stat-tm-bb.qmd | 2 |
| `tm-sim.qmd` | tm-ca.qmd | 1 |
| `tm-null.qmd` | stat-tm-bb.qmd | 1 |
| `edge-deletion.qmd` | cent-edge.qmd | 1 |
| `random.qmd` | cent-random.qmd | 3 |
| `random-cent.qmd` | cent-random.qmd | 1 |
| `edgecent.qmd` | comm-edge.qmd | 1 |

### Notes:
- Some files reference themselves with different names (e.g., `sim-se.qmd` should probably be `sim-structequiv.qmd`)
- Many references are to files that were never created
- Several files appear to be old lecture notes that you've consolidated into single documents

---

## Recommendations

### Priority 1: Broken Markdown File Links (Highest Impact)

These are actual broken navigation links that will confuse readers.

**Options:**
1. **Fix valid references** – Update links that point to existing files with wrong names:
   - `sim-se.qmd` → `sim-structequiv.qmd` (3 links to fix in sim-gen.qmd, sim-loc.qmd)
   - Verify other linked files actually exist or should be consolidated

2. **Remove invalid references** – Delete links to files that don't exist and never will

3. **Create missing files** – If these represent curriculum gaps you want to fill

### Priority 2: Broken Table References

**Recommended:** Remove all 9 `@tbl-*` references from text (same approach as equation/figure cleanup)

Files to update:
- `prest-tm-gen.qmd` (4 refs)
- `tm-fitness.qmd` (1 ref)
- `prest-hits.qmd` (1 ref)
- `stat-qap.qmd` (2 refs)
- `tm-reflections.qmd` (1 ref)

---

## Next Steps

Would you like me to:
1. **Remove all broken table references** (`@tbl-*`) using bulk find-and-replace?
2. **Fix the markdown file links** – identify which links are typos vs. missing files?
3. **Both** – clean up both at once?

