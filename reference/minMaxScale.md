# Min-max scaling

Variable min-max scaling.

## Usage

``` r
minMaxScale(vec)
```

## Arguments

- vec:

  vector of numbers to scale

## Examples

``` r
set.seed(1234)

d <- runif(20,1,10)

minMaxScale(d)
#>  [1] 0.1140205 0.6705092 0.6562580 0.6716909 0.9315948 0.6902164 0.0000000
#>  [8] 0.2440590 0.7184166 0.5522864 0.7485144 0.5859032 0.2989677 1.0000000
#> [15] 0.3094522 0.9057508 0.3027860 0.2815564 0.1939159 0.2437039
```
