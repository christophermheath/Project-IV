install.packages("fast")
library(fast)

freq_cukier(2, i = 1, omega_before = 0)
freq_mcrae82(2, i = 1, omega_before = 0)

freq_cukier(5)
freq_cukier(6)
freq_cukier(7)
freq_cukier(8)

freq_mcrae82(5)
freq_mcrae82(6)
freq_mcrae82(7)
freq_mcrae82(8)

freq_cukier(2)
freq_mcrae82(2)

# 3 7
# 1 9 13
# 5 11 19 23
# 11 21 27 35 39
# 1 21 31 37 45 49
# 17 39 59 69 75 83 87
# 23  55  77  97 107 113 121 125
