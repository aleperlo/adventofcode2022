#!/bin/sh

day_dir=$(printf day%02d $1)
mkdir $day_dir
cp template.hs $day_dir/Main.hs
