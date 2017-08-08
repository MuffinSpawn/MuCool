#include <stdio.h>
#include <stdint.h>
#include <cmath>

#include "Signals.h"
#include "Localize.h"

class ChannelData {
 public:
  ChannelData(size_t rows, size_t columns, double const * const data)
      : rows_(rows), columns_(columns), data_(data) { }
  ~ChannelData() {
    delete[] data_;
  }
  size_t rows() {return rows_;}
  size_t columns() {return columns_;}
  double operator()(size_t row, size_t column) {
    // No bounds checking!
    return data_[row*columns_+column];
 }

 double const * const data_;
 private:
  ChannelData();
  const size_t rows_, columns_;
};

ChannelData * loadChannelData(FILE * file_handle) {
  int32_t rows, columns;
  fread(&rows, sizeof(int32_t), 1, file_handle);
  fread(&columns, sizeof(int32_t), 1, file_handle);

  double * data = new double[rows*columns];
  fread(data, sizeof(double), rows*columns, file_handle);
  /*
  for (size_t row = 0; row < 1; ++row) {
    for (size_t column = 0; column < columns; ++column) {
     printf("%e ", data[row*columns+column]);
    }
    printf("\n");
  }
  */

  return new ChannelData(rows, columns, data);
}

ChannelData * loadMicrophoneCoordinates(FILE * file_handle) {
  int32_t microphones;
  fread(&microphones, sizeof(int32_t), 1, file_handle);

  double * data = new double[microphones*2];
  fread(data, sizeof(double), microphones*2, file_handle);
  for (size_t microphone = 0; microphone < microphones; ++microphone) {
    const double radius = data[microphone*2];
    const double angle = data[microphone*2+1];
    
    data[microphone*2] = radius * cos(angle);
    data[microphone*2+1] = radius * sin(angle);
  }

  return new ChannelData(microphones, 2, data);
}

int main(int argc, char ** argv) {
  // TODO test cross_correlation
  printf("Opening data file %s...\n", argv[1]);
  FILE * file_handle = fopen(argv[1], "rb");
  if (!file_handle) {
    fprintf(stderr, "Unable to open file!");
    return 1;
  }
  ChannelData * signals = loadChannelData(file_handle);
  fclose(file_handle);
  /*
  for (size_t row = 0; row < 1; ++row) {
    for (size_t column = 0; column < signals->columns(); ++column) {
     printf("%e ", signals->data_[row*signals->columns()+column]);
    }
    printf("\n");
  }
  */
  /*
  double auto_corr[10239];
  double const * const signal = &signals.samples[0][60];

  for (size_t column = 0; column < 25; ++column) {
   printf("%e ", signal[column]);
  }
  printf("[\n");

  cross_correlation(signal, 25, signal, 25, auto_corr);
  */

  file_handle = fopen("C:\\Users\\plane\\Desktop\\MTA\\SCM with Cu Windows.cfg", "rb");
  if (!file_handle) {
    fprintf(stderr, "Unable to open file!");
    return 1;
  }
  ChannelData * mic_coordinates = loadMicrophoneCoordinates(file_handle);
  fclose(file_handle);

  const size_t resolution[] = {120, 120};
  const size_t window[] = {60, 30};
  double coordinates[2];

  accumulated_correlation(signals->data_,
                          (signals->rows()/2),
                          signals->columns(),
                          mic_coordinates->data_,
                          resolution,
                          window,
                          coordinates);

  printf("Upstream Coordinates: (%f, %f)", coordinates[0], coordinates[1]);

  delete mic_coordinates;
  delete signals;
  return 0;
}
