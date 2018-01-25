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
box_cox_std(1, 3)
# 0
box_cox_std(2, 3) # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# 0.58333

# A
box_cox_shift_std(1, 3)
# 0
box_cox_shift_std(2, 3) # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# 0.58333


# B 
box_cox_std(2, 0) # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# 1.386 
box_cox_std(3, 0) # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# 3.296 

box_cox_shift_std(2, 0) # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# 1.386 
box_cox_shift_std(3, 0) # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# 3.296 


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
# -0.087 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
modul_back(-2, 3)
# -1.91 

# B
modul_back(2, 0)
# 6.38
modul_back(-2, 0)
# -6.38 

# Standardized transformation

# A
modul_std(2, 3)
# 0.963 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
modul_std(-2, 3)
# -78 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# B
modul_std(2, 0)
# 3.296 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
modul_std(-2, 0)
# -0.366 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


# Bickel-Doksum ################################################################

# Transformation

# A - only one case since lambda can only be positive
Bick_dok(2, 3)
# 8.666
Bick_dok(-2, 3)
# -8.666

# Back-transformation

# A
Bick_dok_back(2, 3)
# 1.91
Bick_dok_back(-2, 3)
# 1.71


# Standardized transformation

# A
Bick_dok_std(2, 3)
# 0.96
Bick_dok_std(-2, 3)
# -78

# Manly ########################################################################

# Transformation

# A
Manly(2, 3)
# 134.1
Manly(-2, 3)
# -0.33

# B
Manly(2, 0)
# 2
Manly(-2, 0)
# -2


# Back-transformation

# A
Manly_back(2, 3)
# 0.648
Manly_back(-2, 3)
# NaN

# B
Manly_back(2, 0)
# 2
Manly_back(-2, 0)
# -2


# Standardized transformation

# A 
Manly_std(2, 3)
# 0.33
Manly_std(-2, 3)
# - 134.1

# B 
Manly_std(2, 0)
# 2
Manly_std(-2, 0)
# -2


# Dual #########################################################################

# Transformation

# A
Dual(2, 3)
# 11.81
Dual(-2, 3)
# -11.81

# B
Dual(2, 0)
# 0.69
Dual(-2, 0)
# NaN


# Back-transformation

# A
Dual_back(2, 3)
# 2.29
Dual_back(-2, 3)
# 0.44

# B
Dual_back(2, 0)
# 7.38
Dual_back(-2, 0)
# 0.135


# Standardized transformation

# A
Dual_std(2, 3)
# 2
Dual_std(-2, 3)
# NaN

# B
Dual_std(2, 0)
# -0.25
Dual_std(-2, 0)
# NaN


# Gpower #######################################################################

# Transformation 

# A
gPower(2, 3)
# 25.004
gPower(-2, 3)
# -0.329

# B
gPower(2, 0)
# 1.44
gPower(-2, 0)
# -1.44


# Standardized transformation

# A
gPower_std(2, 3)
# 0.874
gPower_std(-2, 3)
# -Inf

# B
gPower_std(2, 0)
# 3.22
gPower_std(-2, 0)
# NaN


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
Yeo_john(2, 2)
# 4
# E
Yeo_john(-2, 2)
# -1.098

# Back-transformation

# A
Yeo_john_back(2, 3)
# 2
# B
Yeo_john_back(2, 0)
# 2
# C
Yeo_john_back(2, 3)
# 2
# D
Yeo_john_back(2, 3)
# E
Yeo_john_back(2, 3)












