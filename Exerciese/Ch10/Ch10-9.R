library(ISLR)


# ----------------------- (a) -----------------------
diss1 = dist(USArrests)
hc1 = hclust(diss1, method='complete')
plot(hc1, xlab='', ylab='')


# ----------------------- (b) -----------------------
abline(h=150, col='red')
tree1.2 = cutree(hc1, 3)

g1 = names(tree1.2[ tree1.2==1 ])
# [1] "Alabama"        "Alaska"         "Arizona"       
# [4] "California"     "Delaware"       "Florida"       
# [7] "Illinois"       "Louisiana"      "Maryland"      
# [10] "Michigan"       "Mississippi"    "Nevada"        
# [13] "New Mexico"     "New York"       "North Carolina"
# [16] "South Carolina"
g2 = names(tree1.2[ tree1.2==2 ])
# [1] "Arkansas"      "Colorado"      "Georgia"      
# [4] "Massachusetts" "Missouri"      "New Jersey"   
# [7] "Oklahoma"      "Oregon"        "Rhode Island" 
# [10] "Tennessee"     "Texas"         "Virginia"     
# [13] "Washington"    "Wyoming"    
g3 = names(tree1.2[ tree1.2==3 ])
# [1] "Connecticut"   "Hawaii"        "Idaho"        
# [4] "Indiana"       "Iowa"          "Kansas"       
# [7] "Kentucky"      "Maine"         "Minnesota"    
# [10] "Montana"       "Nebraska"      "New Hampshire"
# [13] "North Dakota"  "Ohio"          "Pennsylvania" 
# [16] "South Dakota"  "Utah"          "Vermont"      
# [19] "West Virginia" "Wisconsin"    


# ----------------------- (c) -----------------------
X = scale(USArrests)
diss2 = dist(X)
hc2 = hclust(diss2, method='complete')
plot(hc2)


# ----------------------- (d) -----------------------
# It should be scaled first
#
# If not scaled, the grouping is primarily driven by the 
# variable with the largest variance.
# Example would be that result in (b) is basically based on
# the variable 'Assualt' alone.

# After scalling, the grouping is based on all four variables
# and produce more balanced results.