#include <stdio.h>
#include <stdint.h>

void cross_correlation(double const * const signal_a, const size_t length_a,
                       double const * const signal_b, const size_t length_b,
                       double * cross_correlation_vector) {
  double const * const x = signal_a;
  double const * const y = signal_b;
  const int n = length_a;
  const int m = length_b;
  /*
  for (int index = 0; index < length_a; ++index) {
    printf("%e ", signal_a[index]);
  }
  printf("\n");
  for (int index = 0; index < length_b; ++index) {
    printf("%e ", signal_b[index]);
  }
  printf("\n");
  */
  // printf("%d x %d", n, m);
  double * const h = cross_correlation_vector + n - 1;
  // for (int j = -(n-1); j < m; ++j) {
  // printf("-(n-1) = %d\tm = %d", -(n-1), m);
  for (int j = -(n-1); j < m; ++j) {
    h[j] = 0.0;
    for (int k = 0; k < n; ++k) {
      int l = j + k;
      // printf("l = %d\t", l);
      if (l < 0) {
        // h[j+n] += 0.0;
        h[j] += 0.0;
      } else if (l >= m) {
        k = m;
      } else {
        h[j] += x[k] * y[l];
        // printf("x[%d] = %e\ty[%d+%d] = %e\th[%d] = %e\n", k, x[k], j, k, y[l], j, h[j]);
      }
    }
    // printf("h[%d] = %e", j, h[j]);
  }
}
