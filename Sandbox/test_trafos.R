################################################################################
# This script compares the manual results with the implemented functions.
################################################################################

# Box-Cox ######################################################################

# Transformation

# A
box_cox(2, 3)
# 2.3333
box_cox(1, 3)
# 0

box_cox_shift(2, 3)
# 2.3333
box_cox_shift(1, 3)
# 0

# B
box_cox(2, 0)
# 0.693
box_cox(1, 0)
# 0

box_cox_shift(2, 0)
# 0.693
box_cox_shift(1, 0)
# 0


# Back-transformation

# A
box_cox_back(2, 3)
# 1.91
box_cox_back(2.33333, 3)
# 1.9999

box_cox_shift_back(2, 3)
# 1.91
box_cox_shift_back(2.33333, 3)
# 1.9999

# B
box_cox_back(2, 0)
# 7.389
box_cox_back(1, 0)
# 2.718

box_cox_shift_back(2, 0)
# 7.389
box_cox_shift_back(1, 0)
# 2.718

# Standardized transformation

# A
y <- c(1, 2)
box_cox_std(y, 3)
# 0 1.166667
# A
box_cox_shift_std(y, 3)
# 0 1.166667

# B 
box_cox_std(y, 0) 
# 0 0.98
box_cox_shift_std(y, 0) 
# 0 0.98

# Modulus ######################################################################

# Transformation

# A
modul(2, 3)
# 8.6666
modul(-2, 3)
# -8.666

# B
modul(2, 0)
# 1.09
modul(-2, 0)
# -1.09

# Back-transformation

# A
modul_back(2, 3)
# 9.91
modul_back(-2, 3)
# -0.91 

# B
modul_back(2, 0)
# 6.38
modul_back(-2, 0)
# -6.38 

# Standardized transformation

# A
y1 <- c(1, 2)
modul_std(y1, 3)
# 0.38 1.44
y2 <- c(-1, -2)
modul_std(y2, 3)
# -0.38 -1.44

# B
modul_std(y1, 0)
# 1.697 2.691
modul_std(y2, 0)
# -1.697 -2.691


# Bickel-Doksum ################################################################

# Transformation

# A - only one case since lambda can only be positive
Bick_dok(2, 3)
# 2.3333
Bick_dok(-2, 3)
# -3

# Back-transformation

# A
Bick_dok_back(2, 3)
# 1.91
Bick_dok_back(2.3333, 3)
Bick_dok_back(-2, 3)
# -1.71
Bick_dok_back(-3, 3)


# Standardized transformation

# A
y1 <- c(1, 2)
Bick_dok_std(y1, 3)
# 0.000000 1.166667
y2 <- c(-1, -2)
Bick_dok_std(y2, 3)
# -0.33 -1.5

# Manly ########################################################################

# Transformation

# A
Manly(2, 3)
# 134.1
Manly(-2, 3)
# -0.3325071
Manly(-1, 3)
# -0.3167376

# B
Manly(2, 0)
# 2
Manly(-2, 0)
# -2


# Back-transformation

# A
Manly_back(2, 3)
# 0.648
Manly_back(134.1, 3)
Manly_back(-0.3325071, 3)

# B
Manly_back(2, 0)
# 2
Manly_back(-2, 0)
# -2


# Standardized transformation

# A 
y1 <- c(1, 2)
Manly_std(y1, 3)
# 0.07067372 1.49019336
y2 <- c(-1, -2)
Manly_std(y2, 3)
# -28.51181 -29.93133

# B 
Manly_std(2, 0)
# 2
Manly_std(-2, 0)
# -2


# Dual #########################################################################

# Transformation

# A
Dual(2, 3)
# 1.3125

# B
Dual(2, 0)
# 0.69
Dual(-2, 0)
# Not defined for negative values!!!

# Back-transformation

# A
Dual_back(2, 3)
# 2.29

# B
Dual_back(2, 0)
# 7.38


# Standardized transformation

# A
y1 <- c(1, 2)
Dual_std(y1, 3)
# 0 0.9209109

# B
Dual_std(y1, 0)
# 0 0.9802581


# Gpower #######################################################################

# Transformation 

# A
gPower(2, 3)
# 25.004

# B
gPower(2, 0)
# 1.44


# Back-transformation

# A
gPower_back(2, 3)
# 0.695

# B
gPower_back(1.443635, 0)
# 1.999999


# Standardized transformation

# A
y1 <- c(1, 2)
gPower_std(y1, 3)
# 0.2369092 1.3595910

# B
gPower_std(y1, 0)
# 1.379424 2.259412



# Yeo-Johnson ##################################################################

# Transformation

# A
Yeo_john(2, 3)
# 8.666
# B
Yeo_john(2, 0)
# 1.09
# C
Yeo_john(-2, 3)
# -0.6666
# D
Yeo_john(-2, 2)
# -1.098

# Back-transformation

# A
Yeo_john_back(8.666, 3)
# 1.999926
# B
Yeo_john_back(1.098612, 0)
# 1.999999
# C
Yeo_john_back(-0.66666, 3)
# -1.99994
# D
Yeo_john_back(-1.098, 2)
# -1.998164

# Standardized transformation

# A
y1 <- c(1, 2)
Yeo_john_std(y1, 3)
# 0.3888889 1.4444444
# B
Yeo_john_std(y1, 0)
# 1.697857 2.691040
# C
y2 <- c(-1, -2)
Yeo_john_std(y2, 3)
# -0.2041241 -0.2721655
# C
Yeo_john_std(y2, 2)
# -1.697857 -2.691040


# Neglog #######################################################################

# Transformation
neg_log(2)
# 1.098612

# Back-transformation
neg_log_back(1.098612)

# Standardized transformation
y1 <- c(1, 2)
neg_log_std(y1)
 

# Square-root shift ############################################################

# Transformation
sqrt_shift(2, 3)
# 2.23

sqrt_shift(-5, 3)
# 1

# Back-transformation
sqrt_shift_back(2.236068, 3)

sqrt_shift_back(1, 6)

# Standardized transformation
sqrt_shift_std(y1, 3)


# Glog #########################################################################

# Transformation
g_log(2)

# Back-transformation
g_log_back(2)

# Standardized transformation
y1 <- c(1, 2)
g_log_std(y1)


# Log-shift opt ################################################################

# Transformation 

# A
log_shift_opt(2, 3)
# 1.609438
log_shift_opt(-5, 3)
# 0

# B
log_shift_opt(2, 0)
# 0.6931472
log_shift_opt(-2, 0)
# 0


# Back-transformation

# A
log_shift_opt_back(1.609438, 3)
# 2
log_shift_opt_back(0, 6)
# -5

# B
log_shift_opt_back(0.6931472, 0)
# 2
log_shift_opt_back(0, 3)
# -2

# Standardized transformation

# A
log_shift_opt_std(y1, 3)
# 8.047
log_shift_opt_std(-2, 3)
# 0

# A
log_shift_opt_std(2, 0)
# 1.386294
log_shift_opt_std(-2, 0)
# 0



