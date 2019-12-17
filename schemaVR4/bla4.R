hypothesis(combinedData_rec_AFC, "objLocTargetRating= 0")
plot(hypothesis(combinedData_rec_AFC, "objLocTargetRating= 0"))

postDist <- posterior_samples(combinedData_rec_AFC)$b_objLocTargetRating

fit.posterior <- logspline(postDist)


# Wagenmakers et al. (2010) order-restricted method 1 based on renormalisation: beta = 0 vs. beta > 0 
posterior     <- dlogspline(0, fit.posterior) 
# renormalize:
area          <- sum(postDist <  0)/length(postDist)
posterior.OR  <- posterior/area
prior.OR      <- 2*dnorm(0 ,0, 1)  
1/(posterior.OR/prior.OR)




intercept1 <- ranef(combinedData_rec_AFC)$`experiment`[1,1,1] + fixef(combinedData_rec_AFC)[1, 1]
slope1     <- ranef(combinedData_rec_AFC)$`experiment`[1,1,2] + fixef(combinedData_rec_AFC)[2, 1]

pred1 <- intercept1 + slope1*combinedData$objLocTargetRating

intercept2 <- ranef(combinedData_rec_AFC)$`experiment`[2, 1, 1] + fixef(combinedData_rec_AFC)[1, 1]
slope2     <- ranef(combinedData_rec_AFC)$`experiment`[2, 1, 2] + fixef(combinedData_rec_AFC)[2, 1]

pred2 <- intercept2 + slope2*combinedData$objLocTargetRating

predictedValues <- data.frame(objLocTargetRating = c(combinedData$objLocTargetRating),
                              pred = c(pred1, pred2),
                              experiment = c(rep('2', length(pred1)), rep('3',length(pred2))))

ggplot(combinedData, aes(x = objLocTargetRating, y = probitRec, colour = experiment)) + 
  geom_point() + 
  geom_line(data = predictedValues, aes(x = objLocTargetRating, y = pred, colour = experiment))
