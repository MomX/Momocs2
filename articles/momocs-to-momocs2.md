# momocs-to-momocs2

``` r
library(Momocs2)
```

    Momocs; Momocs2

    Core trasnformations ----
    coo_center ; same
    coo_centre ; removed
    coo_trans ; now coo_translate
    coo_rotate ; same
    coo_rotatecenter ; now coo_rotate_around
    coo_scale ; same
    coo_scalex/y ; removed
    coo_flipx ; now coo_flip_xaxis (same for y)
    coo_flip_x ;  new function that uses y plane with arbitrary y line (centroid dy default, same for y)
    coo_shearx ; now coo_shear_x (same for y)
    coo_untiltx ; now coo_untilt_x (plus the new coo_untilt_y)

    Alignement ----
    coo_align ; now use centroid as center.
    coo_align_minor ; new uses 2nd svd axes (~2nd PC)
    coo_aligncalliper ; now coo_align_calliper
    coo_alignminradius ; now coo_align_minradius
    coo_alignxax ; removed since redundant
    coo_baseline ; same but beter implementation
    coo_bookstein : same but beter implementation
    coo_slide ; same but now guarantees to handle funny values
    coo_slide_angle ; new function that can slide to any angle
    coo_slidedirection ; now coo_slide_direction (same as coo_slide_angle but with right/up/left/down)
    coo_slide_closest ; new function can slide to closest of an arbitrary point
    coo_slidegap ; now coo_slide_gap

    Sampling & smoothing -----
    coo_sample ; same name but now uses a real approximation along arc length / curvilinear abscissa. Using aprox also allows interpolate.
    coo_sample_prop ; idem
    coo_samplerr ; TODO
    coo_interpolate ; removed (now handled by coo_sample)
    coo_smooth ; same but better implementation
    coo_smoothcurve ; coo_smooth_fixed


    Point manipulation: -----

    coo_trim -> now coo_trim_head. Also new opposite function : coo_head
    coo_trimbottom -> now_coo_trim_tail (new opposite function : coo_tail). Also new coo_trim_both
    coo_extract -> same
    coo_rev ; now coo_reverse
    coo_jitter ; to be implemented in Momfarm
    coo_close ; same
    coo_unclose ; now coo_open
    coo_force2close ; now coo_close_spread


    # RETURN SCALARS
    See the general helper measure()
    Area & perimeter:
    coo_area ; now get_area
    coo_perim ; now get_perim
    coo_perimcum ; now get_perimcum
    coo_perimpts ; now get_perim_along

    Size & dimensions:
    coo_centsize ; now get_centroid_size_norm
    coo_centsize ; new function get_centroid_size (classical version)
    coo_length ; now get_length
    coo_width ; now get_width
    coo_lw ; now get_lw
    coo_calliper ; now get_calliper. Also get_calliper_ids

    Shape descriptors:
    coo_circularity ; now get_circularity
    coo_circularityharalick ; now get_circularity_haralick
    coo_circularitynorm ; now get_circularity_norm
    coo_convexity ; now get_convexity
    coo_solidity ; now get_solidity
    coo_eccentricityboundingbox ; removed
    coo_eccentricityeigen ; removed
    coo_elongation ; now get_elongation
    coo_rectangularity ; now get_rectangularity
    coo_rectilinearity ; now get_rectilinearity

    coo_centdist ; now get_centroid_distance
    coo_nb : now get_coords_nb # just coutn nrow
    coo_range ; now get_range
    coo_diffrange ; now get_range_diff
    coo_angle_edges ; now get_angle_edges
    coo_angle_tangent : now get_angle_tangent
    get_rugosity()       # Perimeter / convex_hull_perimeter
    get_compactness()    # Various compactness indices


    Truss and scalars
    measure_truss            # coo_truss
    measure_scalars          # coo_scalars


    stack -> pile
    panel -> mosaic. Now templates relatively (or not). Also displays first point and
    outline landmarks (if any)

    coo_boundingbox        #  get_bbox
    coo_chull ; get_chull. also get_chull_id
    coo_chull_onion       # get_chull_onion
    coo_ldk               # removed. See Momacs.
    coo_range_enlarge     # removed

    coo_check             ; same
    coo_is_closed         # logical check # TODO but not a coo
    coo_template          # same
    coo_template_relatively  # same

    coo_right ; same
    coo_up ; same
    coo_left ; same
    coo_down ; same

    coo_likely_clockwise ; now get_direction_sign
    coo_likely_clockwise ; now get_direction_sign, 
    new; coo_direction_negative coo_direction_positive

    coo_intersect_angle ; now get_closest_angle
    coo_intersect_direction ; now get_closest_direction
    new   ; get_closest (to arbitrary point)
    coo_intersect_segment ; removed

    coo_close ; make last point to be the same as the first (if not already)
    coo_open ; if last point is also the first, then remove last point

    coo_trim_start ; now coo_trim_head (gains n as fraction)
    coo_trim_end ; now coo_trim_tail (same)
    coo_trim ; now coo_trim_both (same)

    coo_slice ; now get_cut
    new ; get_join

    new ; get_transform (calc a transformation list between two shapes)
    new ; get_trasnformed (apply it)

Yop.
