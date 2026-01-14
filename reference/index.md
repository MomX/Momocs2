# Package index

## Morphometric methods

### Outlines

- [`efourier()`](https://momx.github.io/Momocs2/reference/efourier.md) :
  Elliptic Fourier Transform
- [`efourier_i()`](https://momx.github.io/Momocs2/reference/efourier_i.md)
  : Inverse elliptic Fourier transform
- [`efourier_name()`](https://momx.github.io/Momocs2/reference/efourier_name.md)
  : Name elliptic Fourier coefficients
- [`efourier_norm()`](https://momx.github.io/Momocs2/reference/efourier_norm.md)
  : Normalize elliptic Fourier coefficients
- [`efourier_split()`](https://momx.github.io/Momocs2/reference/efourier_split.md)
  : Split EFT coefficient vector into components

## Classes and printers

Mostly cosmetics

- [`as_coo()`](https://momx.github.io/Momocs2/reference/as_class.md)
  [`as_out()`](https://momx.github.io/Momocs2/reference/as_class.md)
  [`as_ldk()`](https://momx.github.io/Momocs2/reference/as_class.md)
  [`as_xy()`](https://momx.github.io/Momocs2/reference/as_class.md)
  [`as_ldk_id()`](https://momx.github.io/Momocs2/reference/as_class.md)
  [`as_cur()`](https://momx.github.io/Momocs2/reference/as_class.md)
  [`as_path()`](https://momx.github.io/Momocs2/reference/as_class.md)
  [`as_meas()`](https://momx.github.io/Momocs2/reference/as_class.md)
  [`as_coe()`](https://momx.github.io/Momocs2/reference/as_class.md)
  [`as_eft()`](https://momx.github.io/Momocs2/reference/as_class.md)
  [`as_rft()`](https://momx.github.io/Momocs2/reference/as_class.md)
  [`as_dct()`](https://momx.github.io/Momocs2/reference/as_class.md)
  [`as_npoly()`](https://momx.github.io/Momocs2/reference/as_class.md)
  [`as_opoly()`](https://momx.github.io/Momocs2/reference/as_class.md)
  [`as_proc()`](https://momx.github.io/Momocs2/reference/as_class.md) :
  Coerce to Momocs morphometric classes

## Dispatcher and function factories

Exposed extending helpers

- [`get_coo_cols()`](https://momx.github.io/Momocs2/reference/get_coo_cols.md)
  : Identify coo columns in a tibble
- [`make_coo_function()`](https://momx.github.io/Momocs2/reference/make_coo_function.md)
  : Create a coo function with automatic dispatch
- [`make_get_function()`](https://momx.github.io/Momocs2/reference/make_get_function.md)
  : Create a get function with automatic dispatch
- [`get_coe_cols()`](https://momx.github.io/Momocs2/reference/get_coe_cols.md)
  : Identify coe columns in a tibble

## Verbs

ease common operations

- [`fold()`](https://momx.github.io/Momocs2/reference/fold.md) : Fold
  multiple columns into a single list-column
- [`unfold()`](https://momx.github.io/Momocs2/reference/unfold.md) :
  Unfold a list-column into multiple columns or a matrix
- [`front()`](https://momx.github.io/Momocs2/reference/front.md) :
  Relocate morphometric columns to front
- [`declass()`](https://momx.github.io/Momocs2/reference/declass.md) :
  Remove Momocs classes

## Dataset

To play with

- [`bot`](https://momx.github.io/Momocs2/reference/bot.md) : Bottles
  dataset
- [`hearts`](https://momx.github.io/Momocs2/reference/hearts.md) : Heart
  dataset
- [`wings`](https://momx.github.io/Momocs2/reference/wings.md) : Wings
  dataset
- [`shapes`](https://momx.github.io/Momocs2/reference/shapes.md) :
  Shapes dataset

## coo\_\* coordinate transformations

Functions that take coo and return coo

### Basic transformations

- [`coo_center()`](https://momx.github.io/Momocs2/reference/coo_center.md)
  : Center coordinates
- [`coo_translate()`](https://momx.github.io/Momocs2/reference/coo_translate.md)
  : Translate coordinates
- [`coo_scale()`](https://momx.github.io/Momocs2/reference/coo_scale.md)
  : Scale to unit centroid size

### Rotation

- [`coo_rotate()`](https://momx.github.io/Momocs2/reference/coo_rotate.md)
  : Rotate coordinates
- [`coo_rotate_around()`](https://momx.github.io/Momocs2/reference/coo_rotate_around.md)
  : Rotate around a center point
- [`coo_untilt_x()`](https://momx.github.io/Momocs2/reference/coo_untilt.md)
  [`coo_untilt_y()`](https://momx.github.io/Momocs2/reference/coo_untilt.md)
  : Untilt to axis

### Alignment

Align shapes using internal information

- [`coo_align()`](https://momx.github.io/Momocs2/reference/coo_align.md)
  : Align shape to principal axes
- [`coo_align_minor()`](https://momx.github.io/Momocs2/reference/coo_align_minor.md)
  : Align shape to minor and major axes (swapped)
- [`coo_align_calliper()`](https://momx.github.io/Momocs2/reference/coo_align_features.md)
  [`coo_align_minradius()`](https://momx.github.io/Momocs2/reference/coo_align_features.md)
  : Align to special features

### Baseline registration

Align shapes using landmarks

- [`coo_baseline()`](https://momx.github.io/Momocs2/reference/coo_baseline.md)
  [`coo_bookstein()`](https://momx.github.io/Momocs2/reference/coo_baseline.md)
  : Baseline correction (Bookstein registration)

### Sampling and smoothing

Sample up and down coordinates number

- [`coo_sample()`](https://momx.github.io/Momocs2/reference/coo_sample.md)
  [`coo_sample_prop()`](https://momx.github.io/Momocs2/reference/coo_sample.md)
  : Sample coordinates
- [`coo_sample_regular_radius()`](https://momx.github.io/Momocs2/reference/coo_sample_regular_radius.md)
  : Sample at regular angles from centroid
- [`coo_smooth()`](https://momx.github.io/Momocs2/reference/coo_smooth.md)
  [`coo_smooth_fixed()`](https://momx.github.io/Momocs2/reference/coo_smooth.md)
  : Smooth coordinates

### Slide

Change row ordering

- [`coo_slide_id()`](https://momx.github.io/Momocs2/reference/coo_slide.md)
  [`coo_slide_ldk()`](https://momx.github.io/Momocs2/reference/coo_slide.md)
  [`coo_slide_closest()`](https://momx.github.io/Momocs2/reference/coo_slide.md)
  [`coo_slide_angle()`](https://momx.github.io/Momocs2/reference/coo_slide.md)
  [`coo_slide_direction()`](https://momx.github.io/Momocs2/reference/coo_slide.md)
  [`coo_slide_gap()`](https://momx.github.io/Momocs2/reference/coo_slide.md)
  : Slide coordinates along outline

### Direction

Control trigonometric or clockwise direction

- [`coo_reverse()`](https://momx.github.io/Momocs2/reference/coo_reverse.md)
  : Reverse point order
- [`coo_direction_positive()`](https://momx.github.io/Momocs2/reference/coo_direction.md)
  [`coo_direction_negative()`](https://momx.github.io/Momocs2/reference/coo_direction.md)
  : Ensure positive (counter-clockwise) direction

### Open/close shapes

- [`coo_close()`](https://momx.github.io/Momocs2/reference/coo_close.md)
  [`coo_open()`](https://momx.github.io/Momocs2/reference/coo_close.md)
  : Close or open outlines
- [`coo_close_spread()`](https://momx.github.io/Momocs2/reference/coo_close_spread.md)
  : Close outline by spreading gap

### Template shapes

Standardize on bounding boxes

- [`coo_template()`](https://momx.github.io/Momocs2/reference/coo_template.md)
  [`coo_template_relatively()`](https://momx.github.io/Momocs2/reference/coo_template.md)
  : Template shapes to standard size

### Flipping

Reflect shapes

- [`coo_flip_x()`](https://momx.github.io/Momocs2/reference/coo_flip.md)
  [`coo_flip_xaxis()`](https://momx.github.io/Momocs2/reference/coo_flip.md)
  [`coo_flip_y()`](https://momx.github.io/Momocs2/reference/coo_flip.md)
  [`coo_flip_yaxis()`](https://momx.github.io/Momocs2/reference/coo_flip.md)
  : Flip coordinates

### Filtering

Extract coordinates based on spatial criteria

- [`coo_extract()`](https://momx.github.io/Momocs2/reference/coo_extract.md)
  [`coo_head()`](https://momx.github.io/Momocs2/reference/coo_extract.md)
  [`coo_tail()`](https://momx.github.io/Momocs2/reference/coo_extract.md)
  : Extract or subset coordinates
- [`coo_trim_head()`](https://momx.github.io/Momocs2/reference/coo_trim.md)
  [`coo_trim_tail()`](https://momx.github.io/Momocs2/reference/coo_trim.md)
  [`coo_trim_both()`](https://momx.github.io/Momocs2/reference/coo_trim.md)
  : Trim points from outline ends
- [`coo_up()`](https://momx.github.io/Momocs2/reference/coo_direction_filter.md)
  [`coo_down()`](https://momx.github.io/Momocs2/reference/coo_direction_filter.md)
  [`coo_right()`](https://momx.github.io/Momocs2/reference/coo_direction_filter.md)
  [`coo_left()`](https://momx.github.io/Momocs2/reference/coo_direction_filter.md)
  : Filter points by direction from centroid

### Translate variants

- [`coo_translate_jitter()`](https://momx.github.io/Momocs2/reference/coo_translate_jitter.md)
  : Jitter coordinates
- [`coo_translate_to_xaxis()`](https://momx.github.io/Momocs2/reference/coo_translate_to_xaxis.md)
  : Translate shape to x-axis
- [`coo_translate_to_yaxis()`](https://momx.github.io/Momocs2/reference/coo_translate_to_yaxis.md)
  : Translate shape to y-axis

### Cut and join

Cut single shapes into several and back

- [`get_cut()`](https://momx.github.io/Momocs2/reference/get_cut.md) :
  Cut outline at landmarks
- [`get_join()`](https://momx.github.io/Momocs2/reference/get_join.md) :
  Join curves into closed outline

### Misc operations

Where to put them? Why not here?

- [`coo_check()`](https://momx.github.io/Momocs2/reference/coo_check.md)
  : Check and clean coordinates
- [`coo_shear_x()`](https://momx.github.io/Momocs2/reference/coo_shear.md)
  [`coo_shear_y()`](https://momx.github.io/Momocs2/reference/coo_shear.md)
  : Shear coordinates

## get\_\* shape descriptors

Functions that take coo and return scalar descriptors

### Measurement helper

Compute and add multiple measurements

- [`measure()`](https://momx.github.io/Momocs2/reference/measure.md) :
  Measure shape properties
- [`available_measures()`](https://momx.github.io/Momocs2/reference/available_measures.md)
  : List available measurements

### Basic size measures

- [`get_area()`](https://momx.github.io/Momocs2/reference/get_area.md) :
  Get area of a closed outline
- [`get_perim()`](https://momx.github.io/Momocs2/reference/get_perim.md)
  [`get_perim_along()`](https://momx.github.io/Momocs2/reference/get_perim.md)
  [`get_perim_cum()`](https://momx.github.io/Momocs2/reference/get_perim.md)
  : Get perimeter measurements

### Other size measures

Overall size and dimensions

- [`get_length()`](https://momx.github.io/Momocs2/reference/get_length.md)
  : Get length
- [`get_width()`](https://momx.github.io/Momocs2/reference/get_width.md)
  : Get width
- [`get_calliper()`](https://momx.github.io/Momocs2/reference/get_calliper.md)
  [`get_calliper_ids()`](https://momx.github.io/Momocs2/reference/get_calliper.md)
  : Get calliper
- [`get_centroid_size()`](https://momx.github.io/Momocs2/reference/get_centroid_size.md)
  [`centsize()`](https://momx.github.io/Momocs2/reference/get_centroid_size.md)
  : Get centroid size
- [`get_centroid_size_norm()`](https://momx.github.io/Momocs2/reference/get_centroid_size_norm.md)
  [`centsize_norm()`](https://momx.github.io/Momocs2/reference/get_centroid_size_norm.md)
  : Get normalized centroid size

### Circularity and aspect

- [`get_elongation()`](https://momx.github.io/Momocs2/reference/get_elongation.md)
  : Get elongation
- [`get_circularity()`](https://momx.github.io/Momocs2/reference/get_circularity.md)
  [`get_circularity_norm()`](https://momx.github.io/Momocs2/reference/get_circularity.md)
  [`get_circularity_haralick()`](https://momx.github.io/Momocs2/reference/get_circularity.md)
  : Get circularity measures

### Shape complexity

- [`get_convexity()`](https://momx.github.io/Momocs2/reference/get_convexity.md)
  : Get convexity
- [`get_solidity()`](https://momx.github.io/Momocs2/reference/get_solidity.md)
  : Get solidity
- [`get_rugosity()`](https://momx.github.io/Momocs2/reference/get_rugosity.md)
  : Get rugosity
- [`get_compactness()`](https://momx.github.io/Momocs2/reference/get_compactness.md)
  : Get compactness
- [`get_rectangularity()`](https://momx.github.io/Momocs2/reference/get_rectangularity.md)
  : Get rectangularity
- [`get_rectilinearity()`](https://momx.github.io/Momocs2/reference/get_rectilinearity.md)
  : Get rectilinearity

## get\_\* other operations

Functions that take coo and return other than coo and scalars

### Landmarks

ldk_id coordinates extraction

- [`get_ldk()`](https://momx.github.io/Momocs2/reference/get_ldk.md) :
  Extract landmark coordinates

### Transformations

Extract and apply geometric transformations

- [`get_transform()`](https://momx.github.io/Momocs2/reference/get_transform.md)
  : Extract transformation between two configurations
- [`get_transformed()`](https://momx.github.io/Momocs2/reference/get_transformed.md)
  : Apply transformation to coordinates

### Centroid related

- [`get_centroid()`](https://momx.github.io/Momocs2/reference/get_centroid.md)
  : Get centroid of a shape
- [`get_centroid_distance()`](https://momx.github.io/Momocs2/reference/get_centroid_distance.md)
  : Get distances from centroid

### Distances and angles

Distances and angular measures

- [`get_centroid_distance()`](https://momx.github.io/Momocs2/reference/get_centroid_distance.md)
  : Get distances from centroid
- [`get_perim()`](https://momx.github.io/Momocs2/reference/get_perim.md)
  [`get_perim_along()`](https://momx.github.io/Momocs2/reference/get_perim.md)
  [`get_perim_cum()`](https://momx.github.io/Momocs2/reference/get_perim.md)
  : Get perimeter measurements
- [`get_lw()`](https://momx.github.io/Momocs2/reference/get_lw.md) : Get
  length and width
- [`get_calliper()`](https://momx.github.io/Momocs2/reference/get_calliper.md)
  [`get_calliper_ids()`](https://momx.github.io/Momocs2/reference/get_calliper.md)
  : Get calliper
- [`get_truss()`](https://momx.github.io/Momocs2/reference/get_truss.md)
  : Get truss (all pairwise distances)

### Angles

- [`get_angles()`](https://momx.github.io/Momocs2/reference/get_angles.md)
  : Get angles from centroid
- [`get_angle_edges()`](https://momx.github.io/Momocs2/reference/get_angle_edges.md)
  : Get angles at vertices
- [`get_angle_tangent()`](https://momx.github.io/Momocs2/reference/get_angle_tangent.md)
  : Get tangent angles

### Extents and boundaries

- [`get_range()`](https://momx.github.io/Momocs2/reference/get_range.md)
  : Get coordinate ranges
- [`get_range_diff()`](https://momx.github.io/Momocs2/reference/get_range_diff.md)
  : Get range span
- [`get_bbox()`](https://momx.github.io/Momocs2/reference/get_bbox.md) :
  Get bounding box corners
- [`get_coords_nb()`](https://momx.github.io/Momocs2/reference/get_coords_nb.md)
  : Get number of coordinates

### Direction and position

Orientation and point location

- [`get_direction_sign()`](https://momx.github.io/Momocs2/reference/get_direction_sign.md)
  : Get outline direction sign
- [`get_closest()`](https://momx.github.io/Momocs2/reference/get_closest.md)
  [`get_closest_angle()`](https://momx.github.io/Momocs2/reference/get_closest.md)
  : Find closest point to target
- [`get_closest_direction()`](https://momx.github.io/Momocs2/reference/get_closest_direction.md)
  : Get point closest to a given direction

### Convex hull

Convex hull extraction and analysis

- [`get_chull()`](https://momx.github.io/Momocs2/reference/get_chull.md)
  [`get_chull_id()`](https://momx.github.io/Momocs2/reference/get_chull.md)
  : Get convex hull
- [`get_chull_onion()`](https://momx.github.io/Momocs2/reference/get_chull_onion.md)
  : Get convex hull layers (onion peeling)

## Visualization

Plotting and display functions

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

## Reimport

From other packages

- [`pipes`](https://momx.github.io/Momocs2/reference/pipes.md)
  [`%>%`](https://momx.github.io/Momocs2/reference/pipes.md)
  [`%T>%`](https://momx.github.io/Momocs2/reference/pipes.md)
  [`%$%`](https://momx.github.io/Momocs2/reference/pipes.md) : Pipe
  operators
