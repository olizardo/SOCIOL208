# Cross-Reference Cleanup Report
**Generated:** 2026-06-08

## Summary

Your .qmd files have **inconsistent equation and figure referencing**:

| Type | Defined | Referenced | Broken |
|------|---------|-----------|--------|
| **Equations** | 72 | 43 | 43 |
| **Figures** | 0 | 47 | 47 |

**Key Issue:** The 43 equations that are *referenced* in text (using `@eq-xyz` syntax) do **not match** the 72 equation *labels* that are *defined* (using `{#eq-xyz}` syntax). This means either:

1. The text references are pointing to old/renamed labels
2. The equation labels don't match what the text expects
3. Code blocks generating figures lack captions with `{#fig-xyz}` labels

---

## Broken Equation References (43 total)

These equations are referenced in text but have no matching label definition:

```
@eq-bet1           (referenced in: cent-bet.qmd)
@eq-blondel        (referenced in: sim-blondel.qmd)
@eq-bonacich       (referenced in: prest-eigen.qmd)
@eq-brandes2       (referenced in: cent-bet.qmd)
@eq-cos            (referenced in: sim-loc.qmd)
@eq-deg            (referenced in: cent-deg.qmd)
@eq-dice           (referenced in: sim-loc.qmd)
@eq-eclos          (referenced in: cent-edge.qmd)
@eq-eclust         (referenced in: comm-edge.qmd)
@eq-edeg           (referenced in: cent-edge.qmd)
@eq-first-pass-iter (referenced in: basic-random.qmd)
@eq-firstpass      (referenced in: basic-random.qmd)
@eq-fitness        (referenced in: comm-overlap-fitness.qmd)
@eq-flow1          (referenced in: cent-random.qmd)
@eq-g              (referenced in: basic-tm-duality.qmd)
@eq-gen1           (referenced in: prest-tm-gen.qmd)
@eq-gen2           (referenced in: prest-tm-gen.qmd)
@eq-gref0          (referenced in: tm-reflections.qmd)
@eq-gref1          (referenced in: tm-reflections.qmd)
@eq-gref2          (referenced in: tm-reflections.qmd)
@eq-grefq          (referenced in: tm-reflections.qmd)
@eq-ilap           (referenced in: basic-random.qmd)
@eq-iter2          (referenced in: tm-fitness.qmd)
@eq-jac            (referenced in: sim-loc.qmd)
@eq-katz           (referenced in: prest-eigen.qmd)
@eq-nodefit        (referenced in: comm-overlap-fitness.qmd)
@eq-norm1          (referenced in: tm-fitness.qmd)
@eq-norm2          (referenced in: tm-fitness.qmd)
@eq-one            (referenced in: cent-bet.qmd)
@eq-p              (referenced in: basic-tm-duality.qmd)
@eq-pair           (referenced in: cent-bet.qmd)
@eq-pref0          (referenced in: tm-reflections.qmd)
@eq-pref1          (referenced in: tm-reflections.qmd)
@eq-pref2          (referenced in: tm-reflections.qmd)
@eq-prefq          (referenced in: tm-reflections.qmd)
@eq-rclos          (referenced in: cent-random.qmd)
@eq-rep            (referenced in: basic-dir.qmd)
@eq-rolesim        (referenced in: sim-gen.qmd)
@eq-simrank        (referenced in: sim-gen.qmd)
@eq-status-sum     (referenced in: prest-eigen.qmd)
@eq-super          (referenced in: prest-ml.qmd)
@eq-vs             (referenced in: cent-deg.qmd)
@eq-wd             (referenced in: cent-deg.qmd)
```

---

## Broken Figure References (47 total)

**No figures have been labeled with `{#fig-xyz}` syntax anywhere in your .qmd files.**

All 47 figure references are broken:

```
@fig-be-1, @fig-be-2
@fig-ca-1, @fig-ca-2
@fig-cs
@fig-eb-1 through @fig-eb-4, @fig-ebdendro
@fig-fitness-1, @fig-fitness-2
@fig-friends, @fig-friends-2, @fig-friends-3-, @fig-friends-6
@fig-friends2-1, @fig-friends2-2
@fig-friends3-1, @fig-friends3-2
@fig-jw
@fig-krack
@fig-law-1
@fig-mod-1 through @fig-mod-3
@fig-projection
@fig-refs, @fig-refs-1, @fig-refs-2
@fig-rs, @fig-rs2
@fig-se
@fig-sr
@fig-sub
@fig-sw, @fig-sw-1 through @fig-sw-3
@fig-swap
@fig-toy
@fig-wf
@fig-women-1 through @fig-women-4
@fig-yw
```

---

## Recommended Actions

### Option 1: Remove Broken References (Quickest Fix)

If you don't need cross-references, simply remove all `@eq-` and `@fig-` references from your text.

**Files to update:**
- All 25+ files that contain broken equation references
- All files with broken figure references

### Option 2: Add Missing Labels to Equations (Proper Fix)

1. Open each file with broken equation references
2. Find the equation in the document
3. Add/fix the label to match the reference

Example - in `cent-bet.qmd`, the pairwise dependency equation should have:
```latex
$$
\delta_{i(k)j}= \frac{\sigma_{i(k)j}}{\sigma_{ij}}
$$ {#eq-pair}
```

### Option 3: Add Figure Captions with Labels

For every code chunk that produces a figure, add a caption with label:

```markdown
#| fig-cap: "Description of figure"
#| label: fig-example
```

Or in text, after a figure image:

```markdown
![Figure description](path/to/image.png){#fig-example}
```

---

## Next Steps

1. **Decide on approach** (remove, repair, or ignore references)
2. **If repairing:** Start with high-impact files (basic.qmd, cent-deg.qmd, etc. that have many templates)
3. **Use Find & Replace** to systematically update references across files
4. **Test rendering** with `quarto render` or `quarto preview` to verify links work

---

## Files Affected (by broken reference count)

| File | Broke Refs |
|------|-----------|
| sim-gen.qmd | 8 |
| prest-eigen.qmd | 5 |
| basic-random.qmd | 3 |
| cent-bet.qmd | 5 |
| comm-edge.qmd | 5 |
| comm-spectral.qmd | 7 |
| prest-hits.qmd | 4 |
| tm-reflections.qmd | 6 |
| ... (+17 others) | ... |

