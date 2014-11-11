// Adapted from http://stackoverflow.com/questions/1719070/
// NB: for performance, this does an inplace quick select
inline double median(std::vector<double>* x) {
  if (x->empty()) return NAN;

  int size = x->size();
  std::vector<double>::iterator upper = x->begin() + (int) (size / 2);
  std::nth_element(x->begin(), upper, x->end());

  if (size % 2 == 1) {
    return *upper;
  } else {
    std::vector<double>::iterator lower = upper - 1;
    std::nth_element(x->begin(), lower, upper);
    return (*upper + *lower) / 2.0;
  }
}

