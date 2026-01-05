# Pillar support for morphometric data types

These S3 methods provide custom formatting for morphometric data types
when displayed in tibbles via the pillar package.

## Usage

``` r
type_sum.coo(x)

pillar_shaft.coo(x, ...)

type_sum.out(x)

pillar_shaft.out(x, ...)

type_sum.ldk(x)

pillar_shaft.ldk(x, ...)

type_sum.ldk_id(x)

pillar_shaft.ldk_id(x, ...)

type_sum.opoly(x)

pillar_shaft.opoly(x, ...)

type_sum.npoly(x)

pillar_shaft.npoly(x, ...)

type_sum.path(x)

pillar_shaft.path(x, ...)

type_sum.cur(x)

pillar_shaft.cur(x, ...)

type_sum.dct(x)

pillar_shaft.dct(x, ...)

type_sum.eft(x)

pillar_shaft.eft(x, ...)

type_sum.proc(x)

pillar_shaft.proc(x, ...)
```

## Arguments

- x:

  A vector of the appropriate class.

- ...:

  Additional arguments passed to pillar methods.

## Value

- `type_sum.*`: A character string with the abbreviated type name

- `pillar_shaft.*`: A pillar shaft object for formatted display

## Details

These methods are called automatically by tibble/pillar and are not
intended for direct use. They provide compact, colored display of:

- `coo`: coordinate matrices with point count and dimensionality

- `out`: outline coordinates with point count

- `ldk`: landmark coordinates with point count

- `ldk_id`: landmark IDs with count

- `opoly`: open polygon coordinates with point count

- `npoly`: closed polygon lists with polygon count

- `path`: path coordinates with point count

- `cur`: curve coordinates with point count

- `dct`: discrete cosine transform coefficients with range

- `eft`: elliptical Fourier transform coefficients with range

- `proc`: Procrustes-aligned coordinates with point count
