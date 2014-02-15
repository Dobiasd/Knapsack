#!/bin/bash

#./build/Main +RTS -N4

time ./build/Main "test_images/FuBK-Testbild_ascii.pgm" "test/loadsave_ascii.pgm" id ascii
time ./build/Main "test_images/FuBK-Testbild_ascii.ppm" "test/loadsave_ascii.ppm" id ascii

time ./build/Main "test_images/FuBK-Testbild_binary.pgm" "test/loadsave_binary.pgm" id binary
time ./build/Main "test_images/FuBK-Testbild_binary.ppm" "test/loadsave_binary.ppm" id binary

time ./build/Main "test_images/FuBK-Testbild_binary.pgm" "test/grayToRGB.pgm" grayToRGB binary
time ./build/Main "test_images/FuBK-Testbild_binary.ppm" "test/rgbToGray.ppm" rgbToGray binary

time ./build/Main "test_images/FuBK-Testbild_binary.ppm" "test/roi.ppm" roi binary
time ./build/Main "test_images/FuBK-Testbild_binary.ppm" "test/resize.ppm" resize binary
time ./build/Main "test_images/FuBK-Testbild_binary.ppm" "test/resizeProportional.ppm" resizeProportional binary

time ./build/Main "test_images/FuBK-Testbild_binary.ppm" "test/transform_2_points.ppm" transform2 binary
time ./build/Main "test_images/FuBK-Testbild_binary.ppm" "test/transform_3_points.ppm" transform3 binary
time ./build/Main "test_images/FuBK-Testbild_binary.ppm" "test/transform_4_points.ppm" transform4 binary
time ./build/Main "test_images/FuBK-Testbild_binary.ppm" "test/box.ppm" box binary
time ./build/Main "test_images/FuBK-Testbild_binary.ppm" "test/sobelx.ppm" sobelx binary
time ./build/Main "test_images/FuBK-Testbild_binary.ppm" "test/sobely.ppm" sobely binary
time ./build/Main "test_images/FuBK-Testbild_binary.ppm" "test/gauss.ppm" gauss binary