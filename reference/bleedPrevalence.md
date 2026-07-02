# Estimated bleed prevalence (%)

Calculate estimated bleed prevalence.

## Usage

``` r
bleedPrevalence(a, A, b, B, d, s = 3)
```

## Arguments

- a:

  average active bleed size (mm)

- A:

  number of active bleeds

- b:

  average black stain size (mm)

- B:

  number of black stains

- d:

  diameter at breast height (m)

- s:

  height to which stem surveyed from the tree base (m)

## Examples

``` r
bleedPrevalence(30,10,40,5,1,1.3)
#> [1] 0.4162514
```
