cd ../figs
find -X . -name '*.png' -print0 | xargs -0 -n 1 -P 8 optipng -strip all
