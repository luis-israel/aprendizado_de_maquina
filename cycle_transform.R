# given a cyclic ordinal vector x (such as hour of day or day of month) and a period p
# (ie 24 for hour of day), returns a paired sin/cos transformation for the cycle

cycle_transform <- function(x, p) {
  x_sin <- sin(2*pi*x/period)
  x_cos <- cos(2*pi*x/period)
  list(sin = x_sin, cos = x_cos)
}