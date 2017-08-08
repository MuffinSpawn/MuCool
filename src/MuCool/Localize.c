#include <stdio.h>
#include <cstdint>
#include <cmath>
#include <float.h>

#include "Signals.h"
#include "Localize.h"

// --------------------- Function Definitions ----------------------------------

/**
 *  signals is num_channels x signal_length
 *  mic_locations is num_channels x 2
 */
EXPORT void accumulated_correlation(double const * const   signals,
                                    const size_t           num_channels,
                                    const size_t           signal_length,
                                    double const * const   mic_coords,
                                    const size_t           resolution[2],
                                    const size_t           window[2],
                                    double                 coordinates[2]) {
  if (resolution[0] < 1) {
    fprintf(stderr, "The number of candidate x coordintes (nx) must be >= 1\n");
    return;
  } else if (resolution[1] < 1) {
    fprintf(stderr, "The number of candidate y coordintes (ny) must be >= 1\n");
    return;
  }

  const double radius = kCavityRadius;
  const double v_cu = kSpeedOfSoundInCopper;
  const double f_s = kSamplingFrequency;

  // create set of x and y coordinates
  double x_coords[resolution[0]];
  double y_coords[resolution[1]];
  for (size_t index = 0; index < resolution[0]; ++index) {
    x_coords[index] = (1.0 - resolution[0] + 2.0 * index)
                    * radius / resolution[0];
    y_coords[index] = (1.0 - resolution[1] + 2.0 * index)
                    * radius / resolution[1];
  }

  double cross_correlations[num_channels][num_channels][2*window[1]-1];

  // calculate the cross-correlation sums
  double max_sum = -DBL_MAX;
  for (size_t x_index = 0; x_index < resolution[0]; ++x_index) {
    for (size_t y_index = 0; y_index < resolution[1]; ++y_index) {
      const double source_coords[] = {x_coords[x_index], y_coords[y_index]};
      // calculate the sum for the current x and y candidate coordinates
      double sum = 0.0;
      for (size_t i = 0; i < num_channels; ++i) {
        for (size_t j = i+1; j < num_channels; ++j) {
          // calculate the cross-correlation matrix on the first go 'round
          if ((x_index == 0) && (y_index == 0)) {
            /*
            for (size_t a = window[0]; a < (window[0]+window[1]); ++a) {
              printf("%e ", signals[i*signal_length+a]);
              // printf("%e ", signals[j*signal_length+a]);
            }
            */
            cross_correlation(&signals[i*signal_length+window[0]], window[1],
                              &signals[j*signal_length+window[0]], window[1],
                              cross_correlations[i][j]);
            /*
            printf("CC[%d][%d] = \n", i, j);
            for (size_t k = 0; k < (2*window[1]-1); ++k)
              printf("%e ", cross_correlations[i][j][k]);
            printf("\n");
            */
          }

          const double tau_i_q
            = coords_distance(source_coords, &mic_coords[i*2]) / v_cu;
          const double tau_j_q
            = coords_distance(source_coords, &mic_coords[j*2]) / v_cu;
          /*
          printf("Source Coordinates: (%e, %e)", source_coords[0], source_coords[1]);
          printf("i Mic Coordinates: (%e, %e)", mic_coords[i*2], mic_coords[i*2+1]);
          printf("j Mic Coordinates: (%e, %e)", mic_coords[j*2], mic_coords[j*2+1]);
          */
          // printf("Tjq = %e", tau_j_q);
          // printf("Tiq = %e\n", tau_i_q);
          size_t k = window[1] + round((tau_j_q - tau_i_q) * f_s) - 1;
          sum += cross_correlations[i][j][k];
          // printf("%e ", sum);
          // printf("%e ", cross_correlations[i][j][k]);
          // printf("CC[%d][%d][%d] = %e\n", i, j, k, cross_correlations[i][j][k]);
        }
      }
      // printf("sum: %e\n", sum);

      if (sum > max_sum) {
        max_sum = sum;
        coordinates[0] = x_coords[x_index];
        coordinates[1] = y_coords[y_index];
      }
    }
    // printf("\n");
  }
}

double coords_distance(double const * const a, double const * const b) {
  const double delta_x = a[0] - b[0];
  const double delta_y = a[1] - b[1];
  return sqrt(delta_x*delta_x + delta_y*delta_y);
}
