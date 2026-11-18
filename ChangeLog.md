# Changelog

## 1.1.0 (2026-11-18)

**Breaking:** element settings of seven MSGs changed to agree with their BC orientations. Coreps and elements of MSG 17.10, 17.11, 17.13, 19.28, 61.435, 61.438, and 70.529 computed with v1.0.0 no longer match the  data now. *Note that this does **NOT** mean that the data of these seven MSGs are wrong in v1.0.0.* Only settings are changed to make the data more self-consistent. Three of the seven (17.10, 61.435, 70.529) are type-III, so their SymStd/SymBC/CGE assignments now should be added to Table B.3 of the paper [Comput. Phys. Commun. 288, 108722 (2023)]; the  values are tabulated in the README under "Data changes".

### Added

- **`genType3kpairDict[]` and `type3kpairDict2list[]` generate type-III k-point pairing data without calling `getMLGCorep`.** `getType3kpair[]` read the pairing from the precomputed `libMLGCorep`, so regenerating `libMLGCorep` after a change to MSG element data was blocked by a circular dependency. The new pair computes the dictionary directly from the current element data and converts it to `type3kpairList`.

### Changed

- **Element settings of seven MSGs changed to their BC orientations.** `MSGData.wl` (SymBC/CGE and transformation data) and the corresponding `BC_MSG_elements/*.txt` files are updated for 17.10, 17.11, 17.13, 19.28, 61.435, 61.438, and 70.529. All their MSGSymBC changed:

  | No.        | 17.10      | 17.11      | 17.13      | 19.28          | 61.435  | 61.438   | 70.529     |
  | ---------- |:----------:|:----------:|:----------:|:--------------:|:-------:|:--------:|:----------:|
  | SymStd     | $P22'2_1'$ | $P_a222_1$ | $P_A222_1$ | $P_a2_12_12_1$ | $Pb'ca$ | $P_abca$ | $Fd'dd$    |
  | SymBC(old) | $\uparrow$ | $\uparrow$ | $\uparrow$ | $\uparrow$     | $Pc'ab$ | $P_acab$ | $\uparrow$ |
  | SymBC(new) | $P2'22_1'$ | $P_b222_1$ | $P_B222_1$ | $P_b2_12_12_1$ | $Pca'b$ | $P_bcab$ | $Fdd'd$    |
- **`libMLGCorep.mx` and `libMLGCorep.mx_RaspberryPi` regenerated** from the new  data.

### Fixed

- **`showMSGCorep` displays incorrectly for a two-k-star magnetic star  under mathematica >=v13.** The second k-star's section was a nested element of the first line's Row, so its line break and layout were tied to the first k-star's line; the line break is now a standalone element and the second k-star renders as its own line.
