#ifndef MUCOOL_LOCALIZE_H
#define MUCOOL_LOCALIZE_H

#ifdef BUILD_DLL
// the dll exports
#define EXPORT __declspec(dllexport)
#else
// the exe imports
//#define EXPORT __declspec(dllimport)
#define EXPORT 
#endif

// ------------------- Material and Hardware Constants -------------------------
const double kCavityRadius         = 60.0;      // cm
// const double kSpeedOfSoundInCopper = 3.901e5;   // cm/s
const double kSpeedOfSoundInCopper = 4.8e5;   // cm/s
// const double kSpeedOfSoundInCopper = 3.6e5;   // cm/s
const double kSamplingFrequency    = 1.0e5;     // S/s

// --------------------- Function Definitions ----------------------------------

EXPORT void accumulated_correlation(double const * const   signals,
                                    const size_t           num_channels,
                                    const size_t           signal_length,
                                    double const * const   mic_coords,
                                    const size_t           resolution[2],
                                    const size_t           window[2],
                                    double                 coordinates[2]);

double coords_distance(double const * const a, double const * const b);

#endif
