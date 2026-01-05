# Package index

## Core Data Structures

S3 classes and constructor functions for morphometric data types.

## Datasets

Small datasets to play with

- [`bot`](https://momx.github.io/Momocs2/reference/bot.md) : Bottles
  dataset
- [`hearts`](https://momx.github.io/Momocs2/reference/hearts.md) : Heart
  dataset
- [`shapes`](https://momx.github.io/Momocs2/reference/shapes.md) :
  Shapes dataset
- [`wings`](https://momx.github.io/Momocs2/reference/wings.md) : Wings
  dataset

## Coordinate Transformations

Functions to transform shape coordinates. These modify the coo column in
tibbles.

- [`bot`](https://momx.github.io/Momocs2/reference/bot.md) : Bottles
  dataset
- [`hearts`](https://momx.github.io/Momocs2/reference/hearts.md) : Heart
  dataset
- [`p()`](https://momx.github.io/Momocs2/reference/p.md)
  [`draw_landmarks()`](https://momx.github.io/Momocs2/reference/p.md)
  [`draw_landmarks_as_numbers()`](https://momx.github.io/Momocs2/reference/p.md)
  [`draw_outlines()`](https://momx.github.io/Momocs2/reference/p.md)
  [`draw_centroid()`](https://momx.github.io/Momocs2/reference/p.md)
  [`draw_first_point()`](https://momx.github.io/Momocs2/reference/p.md)
  [`draw_links()`](https://momx.github.io/Momocs2/reference/p.md) :
  Minimal plotting system for morphometric shapes
- [`shapes`](https://momx.github.io/Momocs2/reference/shapes.md) :
  Shapes dataset
- [`wings`](https://momx.github.io/Momocs2/reference/wings.md) : Wings
  dataset

## Landmark Registration

Specialized functions for landmark-based alignment.

- [`coo_baseline()`](https://momx.github.io/Momocs2/reference/coo_baseline.md)
  [`coo_bookstein()`](https://momx.github.io/Momocs2/reference/coo_baseline.md)
  : Baseline correction (Bookstein registration)

## Information Extraction

Extract information from shapes without modifying them. Use within
mutate() to add columns.

- [`get_centroid()`](https://momx.github.io/Momocs2/reference/get_centroid.md)
  [`centroid()`](https://momx.github.io/Momocs2/reference/get_centroid.md)
  : Get centroid of a shape
- [`get_centroid_size()`](https://momx.github.io/Momocs2/reference/get_centroid_size.md)
  [`centsize()`](https://momx.github.io/Momocs2/reference/get_centroid_size.md)
  : Get centroid size
- [`get_centroid_size_norm()`](https://momx.github.io/Momocs2/reference/get_centroid_size_norm.md)
  [`centsize_norm()`](https://momx.github.io/Momocs2/reference/get_centroid_size_norm.md)
  : Get normalized centroid size

## Shape Measurements

Measure scalar properties of shapes. Automatically adds columns to
tibbles.

## Visualization

Functions for plotting and visualizing morphometric data.

- [`pile()`](https://momx.github.io/Momocs2/reference/pile.md) : Plot
  shapes stacked together
- [`mosaic()`](https://momx.github.io/Momocs2/reference/mosaic.md) :
  Plot shapes in grid mosaic layout
- [`p()`](https://momx.github.io/Momocs2/reference/p.md)
  [`draw_landmarks()`](https://momx.github.io/Momocs2/reference/p.md)
  [`draw_landmarks_as_numbers()`](https://momx.github.io/Momocs2/reference/p.md)
  [`draw_outlines()`](https://momx.github.io/Momocs2/reference/p.md)
  [`draw_centroid()`](https://momx.github.io/Momocs2/reference/p.md)
  [`draw_first_point()`](https://momx.github.io/Momocs2/reference/p.md)
  [`draw_links()`](https://momx.github.io/Momocs2/reference/p.md) :
  Minimal plotting system for morphometric shapes

## Utilities

Helper functions and utilities.

- [`get_coo_cols()`](https://momx.github.io/Momocs2/reference/get_coo_cols.md)
  : Identify coo columns in a tibble
- [`rinse()`](https://momx.github.io/Momocs2/reference/rinse.md) : Clear
  the environment
- [`` `%>%` ``](https://momx.github.io/Momocs2/reference/pipe.md) : Pipe
  operator

## S3 Methods

Print and display methods for custom classes.
