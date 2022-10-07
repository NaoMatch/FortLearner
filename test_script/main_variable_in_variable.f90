program main_variable_in_variable
    use mod_variable_in_variable
    implicit none

    type(variable_in_variable) :: viv, viv2
    real(kind=8) :: s, v(3), m(4,3), mm(3,3)

    s = 5
    v = 2
    m = 3
    mm = 4

    print*, '*********************************************************************************************'
    print*, "VIV(SCALAR) + SCALAR"
    print*, '*********************************************************************************************'
    viv = s
    print*, "viv = s"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = viv + viv
    print*, "viv = viv + viv"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    viv = s + viv
    print*, "viv = s + viv"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    viv = viv + s
    print*, "viv = viv + s"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = viv - viv
    print*, "viv = viv - viv"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    viv = s - viv
    print*, "viv = s - viv"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    viv = viv - s
    print*, "viv = viv - s"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = viv / viv
    print*, "viv = viv / viv"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    viv = s / viv
    print*, "viv = s / viv"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    viv = viv / s
    print*, "viv = viv / s"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
      print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = viv * viv
    print*, "viv = viv * viv"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    viv = s * viv
    print*, "viv = s * viv"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    viv = viv * s
    print*, "viv = viv * s"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = viv**4_8
    print*, "viv = viv**4_8"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = -s
    viv = abs(viv)
    print*, "viv = abs(viv)"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = tanh(viv)
    print*, "viv = tanh(viv)"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = tan(viv)
    print*, "viv = tan(viv)"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = sqrt(viv)
    print*, "viv = sqrt(viv)"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = sqrt(viv)
    print*, "viv = sqrt(viv)"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = sinh(viv)
    print*, "viv = sinh(viv)"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = sin(viv)
    print*, "viv = sin(viv)"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = log(viv)
    print*, "viv = log(viv)"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = log10(viv)
    print*, "viv = log10(viv)"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = exp(viv)
    print*, "viv = exp(viv)"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = cosh(viv)
    print*, "viv = cosh(viv)"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = cos(viv)
    print*, "viv = cos(viv)"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = atan(viv)
    print*, "viv = atan(viv)"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = .5d0
    viv = asin(viv)
    print*, "viv = asin(viv)"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = .5d0
    viv = acos(viv)
    print*, "viv = acos(viv)"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = floor(viv)
    print*, "viv = floor(viv)"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = ceiling(viv)
    print*, "viv = ceiling(viv)"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    

    print*, '*********************************************************************************************'
    print*, "VIV(SCALAR) + VECTOR"
    print*, '*********************************************************************************************'
    viv = s
    print*, "viv = s"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = viv + v
    print*, "viv = viv + v"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    viv = s
    viv = v + viv
    print*, "viv = v + viv"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = viv - v
    print*, "viv = viv - v"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    viv = s
    viv = v - viv
    print*, "viv = v - viv"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = viv * v
    print*, "viv = viv * v"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    viv = s
    viv = v * viv
    print*, "viv = v * viv"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = viv / v
    print*, "viv = viv / v"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    viv = s
    viv = v / viv
    print*, "viv = v / viv"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
  

    print*, '*********************************************************************************************'
    print*, "VIV(SCALAR) + MATRIX"
    print*, '*********************************************************************************************'
    viv = s
    print*, "viv = s"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = viv + m
    print*, "viv = viv + m"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    viv = s
    viv = m + viv
    print*, "viv = m + viv"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = viv - m
    print*, "viv = viv - m"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    viv = s
    viv = m - viv
    print*, "viv = m - viv"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = viv * m
    print*, "viv = viv * m"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    viv = s
    viv = m * viv
    print*, "viv = m * viv"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = viv / m
    print*, "viv = viv / m"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    viv = s
    viv = m / viv
    print*, "viv = m / viv"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
  

    print*, '*********************************************************************************************'
    print*, "VIV(VECTOR) + SCALAR"
    print*, '*********************************************************************************************'
    viv = v
    print*, "viv = v"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v
    viv = s + viv
    print*, "viv = s + viv"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    viv = v
    viv = viv + s
    print*, "viv = viv + s"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v
    viv = s - viv
    print*, "viv = s - viv"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    viv = v
    viv = viv - s
    print*, "viv = viv - s"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v
    viv = s / viv
    print*, "viv = s / viv"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    viv = v
    viv = viv / s
    print*, "viv = viv / s"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v
    viv = s * viv
    print*, "viv = s * viv"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    viv = v
    viv = viv * s
    print*, "viv = viv * s"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v
    viv = viv**4_8
    print*, "viv = viv**4_8"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = -v
    viv = abs(viv)
    print*, "viv = abs(viv)"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v
    viv = tanh(viv)
    print*, "viv = tanh(viv)"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v
    viv = tan(viv)
    print*, "viv = tan(viv)"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v
    viv = sqrt(viv)
    print*, "viv = sqrt(viv)"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v
    viv = sqrt(viv)
    print*, "viv = sqrt(viv)"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v
    viv = sinh(viv)
    print*, "viv = sinh(viv)"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v
    viv = sin(viv)
    print*, "viv = sin(viv)"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v
    viv = log(viv)
    print*, "viv = log(viv)"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v
    viv = log10(viv)
    print*, "viv = log10(viv)"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v
    viv = exp(viv)
    print*, "viv = exp(viv)"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v
    viv = cosh(viv)
    print*, "viv = cosh(viv)"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v
    viv = cos(viv)
    print*, "viv = cos(viv)"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v
    viv = atan(viv)
    print*, "viv = atan(viv)"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v*.01d0
    viv = asin(viv)
    print*, "viv = asin(viv)"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v*.01d0
    viv = acos(viv)
    print*, "viv = acos(viv)"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v
    viv = floor(viv)
    print*, "viv = floor(viv)"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v
    viv = ceiling(viv)
    print*, "viv = ceiling(viv)"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v
    viv = sum(viv)
    print*, "viv = sum(viv)"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = spread(viv, dim=1, ncopies=10)
    print*, "spread(viv, dim=1, ncopies=10)"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype

    print*, '*********************************************************************************************'
    print*, "VIV(VECTOR) + VECTOR"
    print*, '*********************************************************************************************'
    viv = v
    print*, "viv = v"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v
    viv = v + viv
    print*, "viv = v + viv"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    viv = v
    viv = viv + v
    print*, "viv = viv + v"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v
    viv = v - viv
    print*, "viv = v - viv"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    viv = v
    viv = viv - v
    print*, "viv = viv - v"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v
    viv = v / viv
    print*, "viv = v / viv"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    viv = v
    viv = viv / v
    print*, "viv = viv / v"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v
    viv = v * viv
    print*, "viv = v * viv"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    viv = v
    viv = viv * v
    print*, "viv = viv * v"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype

    print*, '*********************************************************************************************'
    print*, "VIV(VECTOR) + matrix"
    print*, '*********************************************************************************************'
    viv = v
    print*, "viv = v"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v
    viv = m + viv
    print*, "viv = m + viv"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    viv = v
    viv = viv + m
    print*, "viv = viv + m"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v
    viv = m - viv
    print*, "viv = m - viv"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    viv = v
    viv = viv - m
    print*, "viv = viv - m"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v
    viv = m / viv
    print*, "viv = m / viv"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    viv = v
    viv = viv / m
    print*, "viv = viv / m"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = v
    viv = m * viv
    print*, "viv = m * viv"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    viv = v
    viv = viv * m
    print*, "viv = viv * m"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
  


    print*, '*********************************************************************************************'
    print*, "VIV(MATRIX) + SCALAR"
    print*, '*********************************************************************************************'
    viv = m
    print*, "viv = m"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = s + viv
    print*, "viv = s + viv"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    viv = m
    viv = viv + s
    print*, "viv = viv + s"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = s - viv
    print*, "viv = s - viv"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    viv = m
    viv = viv - s
    print*, "viv = viv - s"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = s / viv
    print*, "viv = s / viv"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    viv = m
    viv = viv / s
    print*, "viv = viv / s"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = s * viv
    print*, "viv = s * viv"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    viv = m
    viv = viv * s
    print*, "viv = viv * s"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = viv**4_8
    print*, "viv = viv**4_8"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = -m
    viv = abs(viv)
    print*, "viv = abs(viv)"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = tanh(viv)
    print*, "viv = tanh(viv)"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = tan(viv)
    print*, "viv = tan(viv)"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = sqrt(viv)
    print*, "viv = sqrt(viv)"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = sqrt(viv)
    print*, "viv = sqrt(viv)"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = sinh(viv)
    print*, "viv = sinh(viv)"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = sin(viv)
    print*, "viv = sin(viv)"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = log(viv)
    print*, "viv = log(viv)"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = log10(viv)
    print*, "viv = log10(viv)"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = exp(viv)
    print*, "viv = exp(viv)"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = cosh(viv)
    print*, "viv = cosh(viv)"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = cos(viv)
    print*, "viv = cos(viv)"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = atan(viv)
    print*, "viv = atan(viv)"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m*.01d0
    viv = asin(viv)
    print*, "viv = asin(viv)"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m*.01d0
    viv = acos(viv)
    print*, "viv = acos(viv)"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = floor(viv)
    print*, "viv = floor(viv)"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = ceiling(viv)
    print*, "viv = ceiling(viv)"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = sum(viv)
    print*, "viv = sum(viv)"
    print*, "    s:     ", viv%s
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = sum(viv, dim=1)
    print*, "viv = sum(viv)"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = sum(viv, dim=2)
    print*, "viv = sum(viv)"
    print*, "    v:     ", viv%v
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = spread(spread(viv, dim=1, ncopies=3), dim=1, ncopies=3)
    print*, "spread(viv, dim=1, ncopies=3)"
    print*, "    v:     ", viv%m
    print*, "    v:     ", shape(viv%m)
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = s
    viv = spread(spread(viv, dim=1, ncopies=3), dim=2, ncopies=3)
    print*, "spread(viv, dim=1, ncopies=3)"
    print*, "    v:     ", viv%m
    print*, "    v:     ", shape(viv%m)
    print*, "    dtype: ", viv%dtype

    print*, '*********************************************************************************************'
    print*, "VIV(MATRIX) + VECTOR"
    print*, '*********************************************************************************************'
    viv = m
    print*, "viv = m"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = v + viv
    print*, "viv = v + viv"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    viv = m
    viv = viv + v
    print*, "viv = viv + v"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = v - viv
    print*, "viv = v - viv"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    viv = m
    viv = viv - v
    print*, "viv = viv - v"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = v / viv
    print*, "viv = v / viv"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    viv = m
    viv = viv / v
    print*, "viv = viv / v"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = v * viv
    print*, "viv = v * viv"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    viv = m
    viv = viv * v
    print*, "viv = viv * v"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype

    print*, '*********************************************************************************************'
    print*, "VIV(MATRIX) + matrix"
    print*, '*********************************************************************************************'
    viv = m
    print*, "viv = m"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = m + viv
    print*, "viv = m + viv"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    viv = m
    viv = viv + m
    print*, "viv = viv + m"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = m - viv
    print*, "viv = m - viv"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    viv = m
    viv = viv - m
    print*, "viv = viv - m"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = m / viv
    print*, "viv = m / viv"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    viv = m
    viv = viv / m
    print*, "viv = viv / m"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv = m * viv
    print*, "viv = m * viv"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    viv = m
    viv = viv * m
    print*, "viv = viv * m"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype
    print*, '---------------------------------------------------------------------------------------------'
    viv = m
    viv2 = mm
    viv = matmul(viv, viv2)
    print*, "viv = matmul(viv, viv2)"
    print*, "    m:     ", viv%m
    print*, "    dtype: ", viv%dtype




end program main_variable_in_variable