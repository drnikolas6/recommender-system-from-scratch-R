#train UBCF cosine similarity models
ratingmat=dcast(users_profile,UserID~MovieID,value.var = 'Rating', na.rm=FALSE)
ratingmat=as.matrix(ratingmat[,-1])
ratingmat=as(ratingmat,"realRatingMatrix")
e=evaluationScheme(ratingmat,method='split',train=0.8,given=20,goodRating=4)
###User-Based Collaborative Filtering: Cosine Similarity

# non-normalized
UBCF_N_C <- Recommender(getData(e, "train"), "UBCF", 
                        param=list(normalize = NULL, method="Cosine"))



# compute predicted ratings
p1 <- predict(UBCF_N_C, getData(e, "known"), type="ratings")


# set all predictions that fall outside the valid range to the boundary values
p1@data@x[p1@data@x[] < 0] <- 0
p1@data@x[p1@data@x[] > 5] <- 5


# aggregate the performance statistics
error_UCOS <- rbind(
  UBCF_N_C = calcPredictionAccuracy(p1, getData(e, "unknown"))
)

# memory cleanup
rm(UBCF_N_C, UBCF_C_C, UBCF_Z_C)



#################################################################################
#Item-Based Collaborative Filtering: Cosine Similarity

#train IBCF cosine similarity models

# non-normalized
IBCF_N_C <- Recommender(getData(e, "train"), "IBCF", 
                        param=list(normalize = NULL, method="Cosine"))


# compute predicted ratings
p1 <- predict(IBCF_N_C, getData(e, "known"), type="ratings")


# set all predictions that fall outside the valid range to the boundary values
p1@data@x[p1@data@x[] < 0] <- 0
p1@data@x[p1@data@x[] > 5] <- 5

# aggregate the performance statistics
error_ICOS <- rbind(
  IBCF_N_C = calcPredictionAccuracy(p1, getData(e, "unknown"))
)


# memory cleanup
rm(IBCF_N_C, IBCF_C_C, IBCF_Z_C)

#The table and barplot below summarize the performance of each of the 2 models evaluated above, 
#with the models sorted in ascending order according to their respective RMSE scores.

c_res <- data.frame(rbind(error_UCOS, error_ICOS))

c_res <- c_res[order(c_res$RMSE ),]


# las = 3: rotate x axis labels to perendicular; las = 1: rotate y axis labels
barplot(c_res$RMSE, col = "yellow", main = "Barplot of Model RMSE's", las = 2, ylab = "RMSE", 
        horiz = FALSE, names.arg = rownames(c_res), cex.names=.8)

# get confusion matrices and evaluate accuracy, precision and recall
results=evaluate(x = e,method = "UBCF", type='topNList',n=10)
results@results[[1]]
ubcf_conf_matr_eval=getConfusionMatrix(results)[[1]]
ubcf_acc=sum(ubcf_conf_matr_eval[,c(1,4)])/sum(ubcf_conf_matr_eval[,1:4])

ibcf_results=evaluate(e,"IBCF",'topNList',n=10)
ibcf_conf_matr_eval=getConfusionMatrix(ibcf_results)[[1]]
ibcf_acc=sum(ibcf_conf_matr_eval[,c(1,4)])/sum(ibcf_conf_matr_eval[,1:4])
#create all evaluation matrices
conf_matr_eval=rbind(ibcf_conf_matr_eval,ubcf_conf_matr_eval)
conf_matr_eval=cbind(conf_matr_eval,c(ibcf_acc,ubcf_acc))

rec_lab_evaluation_results=cbind(c_res[,1],conf_matr_eval[,c(9,5,6)])
colnames(rec_lab_evaluation_results)=c('RMSE','Accuracy','Presicion','Recall')
rownames(rec_lab_evaluation_results)=c('IBCF','UBCF')
rec_lab_evaluation_results=as.data.frame(rec_lab_evaluation_results)
colnames(evaluation_results)=c('RMSE','Accuracy','Presicion','Recall')

overall_evaluation=(rbind(rec_lab_evaluation_results,evaluation_results))

class(overall_evaluation)
typeof(overall_evaluation[,4])

#plot results
ggplot(data=overall_evaluation,aes((evaluation_results[1:4,7]),(evaluation_results[1:4,7])))+
  geom_col()

barplot(overall_evaluation[1:5,1], col = "yellow", main = "Barplot of Model RMSE's", las = 2, ylab = "RMSE", 
        horiz = FALSE, names.arg = rownames(overall_evaluation[1:5,]), cex.names=.8)

barplot(overall_evaluation[1:5,2], col = "yellow", main = "Barplot of Model Accuracy", las = 2, ylab = "Acccuracy", 
        horiz = FALSE, names.arg = rownames(overall_evaluation[1:5,]), cex.names=.8)

barplot(overall_evaluation[1:5,3], col = "yellow", main = "Barplot of Model Precision", las = 2, ylab = "Precision", 
        horiz = FALSE, names.arg = rownames(overall_evaluation[1:5,]), cex.names=.8)

barplot(overall_evaluation[1:5,4], col = "yellow", main = "Barplot of Model Recall", las = 2, ylab = "Recall", 
        horiz = FALSE, names.arg = rownames(overall_evaluation[1:5,]), cex.names=.8)
