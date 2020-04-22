class(users_profile)
x=users_profile[,.(.N),by='UserID']
summary(x)
y=users_profile[,.(.N),by='MovieID']
summary(y)#here we can see that ibcf recommender will not be so accurate cause it's
#affected from the movies that are rated less frequently

###transpose profile matrix
#and convert it to a real rating matrix
profile_matrix_2t=t(profile_matrix_2)
profile_matrix_2t=ifelse(profile_matrix_2t==0,NA,profile_matrix_2t)
profile_matrix_2t=as(profile_matrix_2t,'realRatingMatrix')

###split in train test set###
set.seed(10)
#e=evaluationScheme(t(profile_matrix_2),method='split',train=0.7,given=15,goodRating=4)
e2=evaluationScheme(profile_matrix_2t,method='split',train=0.8,given=20,goodRating=4)
e2
train=getData(e2,'train')
test_pred=getData(e2,'known')
test_eval=getData(e2,'unknown')
train=as(train@data,'Matrix')
test_pred=as(test_pred@data,'Matrix')
test_eval=as(test_eval@data,'Matrix')
train=as.matrix(train)
test_pred=as.matrix(test_pred)
test_eval=as.matrix(test_eval)
dim(train)
dim(test_pred)
dim(test_eval)
test_eval[1:10,1:10]
#train[order(rownames(train))[1:10],1:10]
#5857%in%e2@data@data@i
ttrain=t(train)
test_users_id=as.integer(rownames(test_pred[rowSums(test_pred)!=0,]))
ttest=t(test_pred[7,])
###k-fold cross validation###
#kfold=evaluationScheme(profile_matrix_2t,method='cross-validation',train=0.9,k=5,
#                       given=-1,goodRating=4)
#knownratings=as(kfold@knownData@data,'Matrix')
#unknownratings=as(kfold@unknownData@data,'Matrix')
#1st iteration folds
#k_1_train_fold=knownratings[kfold@runsTrain[[1]],]
#k_1_test_fold=knownratings[-kfold@runsTrain[[1]],]
#actual_ratings_k_1=unknownratings[-kfold@runsTrain[[1]]]

####train recommenders

rec2=recommender('UBCF',ttrain)#change name to ubcf_rec
ibcf_rec=recommender('IBCF',ttrain)
cb_rec=recommender('CB',ttrain,genre_matrix_upd)
#pred=pred_function('UBCF',ttest,ttrain,rec2,genre_matrix_upd,movies_upd,'k_nn',30,1,0,10)
#pred$user_preds[order(pred$user_preds[,2],decreasing = T),][1:10,]
#pred$user_preds[1,]
#test_eval[7,858]

####Get predictions and recommendations

##some checks before implementation(include them if you want)
dim(pred$user_preds[,2])
dim(t(pred$top_n_pred))
pred$user_preds[1:3,]
pred_matr[2,]=t(pred$user_preds[,2])
test_pred[1:10,1:10]
#ubcf k_nn method
ubcf_knn_pred_matr=matrix(NA,length(test_users_id),3952)#change name of the matrices
ubcf_knn_recom_matr=matrix(NA,length(test_users_id),10)
for (i in 1:length(test_users_id)){
  ttest=t(test_pred[i,])
  ubcf_knn_pred=pred_function('UBCF',ttest,ttrain,rec2,genre_matrix_upd,movies_upd,'k_nn',30,1,0,10)
  ubcf_knn_pred_matr[i,]=t(ubcf_knn_pred$ubcf_preds[,2])
  ubcf_knn_recom_matr[i,]=t(ubcf_knn_pred$top_n_pred)
}
ubcf_knn_pred_matr[1:10,1:10]#check
ubcf_knn_recom_matr[1:10,]#check
#ubcf threshold method
ubcf_thr_pred_matr=matrix(NA,length(test_users_id),3952)
ubcf_thr_recom_matr=matrix(NA,length(test_users_id),10)
for (i in 1:length(test_users_id)){
  ttest=t(test_pred[i,])
  ubcf_thr_pred=pred_function('UBCF',ttest,ttrain,rec2,genre_matrix_upd,movies_upd,'threshold',30,1,0.1,10)
  ubcf_thr_pred_matr[i,]=t(ubcf_thr_pred$ubcf_preds[,2])
  ubcf_thr_recom_matr[i,]=t(ubcf_thr_pred$top_n_pred)
}
ubcf_thr_pred_matr[1:10,1:10]
ubcf_thr_recom_matr[1:10,1:10]
#ibcf threshold method
ibcf_pred_matr=matrix(NA,length(test_users_id),3952)
ibcf_recom_matr=matrix(NA,length(test_users_id),10)
for (i in 1:length(test_users_id)){
  ttest=t(test_pred[i,])
  ibcf_pred=pred_function('IBCF',ttest,ttrain,ibcf_rec,genre_matrix_upd,movies_upd,'threshold',30,1,0.4,10)
  ibcf_pred_matr[i,]=t(ibcf_pred$ibcf_preds[,2])
  ibcf_recom_matr[i,]=t(ibcf_pred$top_n_pred)
}
ibcf_pred_matr[1:10,1:10]
ibcf_recom_matr[1:10,1:10]
#content based method
cb_pred_matr=matrix(NA,length(test_users_id),3952)
cb_recom_matr=matrix(NA,length(test_users_id),10)
cb_top_n_movies=matrix(NA,length(test_users_id),10)
for (i in 1:length(test_users_id)){
  ttest=t(test_pred[i,])
  cb_pred=pred_function('CB',ttest,ttrain,cb_rec,genre_matrix_upd,movies_upd,'threshold',30,1,0.5,10)
  cb_pred_matr[i,]=t(cb_pred$cb_preds)
  cb_recom_matr[i,]=t(cb_pred$top_n_recs[,2])
  cb_top_n_movies[i,]=t(cb_pred$movies_titles)
}
cb_pred_matr[1:10,1:10]
cb_recom_matr[1:10,1:10]
cb_top_n_movies[1:10,1:10]
#x=list()
#for (i in 1:length(test_users_id)){
#  ttest=t(test_pred[i,])
#  x[[i]]=pred_function('CB',ttest,ttrain,cb_rec,genre_matrix_upd,movies_upd,'threshold',30,1,0.5,10)
#}
#x[[1]]$cb_preds

###Evaluation
###RSME calculation
#ubcf k_nn method
ubcf_pred_values=ubcf_knn_pred_matr
ubcf_pred_values[test_eval==0]=0
ubcf_pred_error=test_eval - ubcf_pred_values
sq_ubcf_error=ubcf_pred_error^2
sum_sq_ubcf_error=sum((sq_ubcf_error))
N=length(test_eval[test_eval!=0])
ubcf_RMSE=sqrt(sum_sq_ubcf_error/N)
#ubcf threshold method
ubcf_thr_pred_values=ubcf_thr_pred_matr
ubcf_thr_pred_values[test_eval==0]=0
ubcf_thr_RMSE=sqrt(sum((test_eval - ubcf_thr_pred_values)^2)/N)
#ibcf
ibcf_pred_values=ibcf_pred_matr
ibcf_pred_values[is.nan(ibcf_pred_values)]=0####
ibcf_pred_values[test_eval==0]=0
ibcf_RMSE=sqrt(sum((test_eval - ibcf_pred_values)^2)/N)
#content based
cb_pred_values=cb_pred_matr
cb_pred_values[test_eval==0]=0
cb_RMSE=sqrt(sum((test_eval - cb_pred_values)^2)/N)
RMSE_matrix=matrix(c(ubcf_RMSE,ubcf_thr_RMSE,ibcf_RMSE,cb_RMSE),4,1)
#results in a matrix
colnames(RMSE_matrix)='RMSE'
rownames(RMSE_matrix)=c('UBCF/k-nn','UBCF/threshold','IBCF/threshold','Content Based')

#accuracy/precision/recall
#ubcf k-nn method
ubcf_pred_values[ubcf_pred_values==0]=NA

TP_ubcf_knn=ubcf_pred_values[test_eval>3 & !is.na(ubcf_pred_values)]
TP_ubcf_knn=length(TP_ubcf_knn[TP_ubcf_knn>3])
TN_ubcf_knn=ubcf_pred_values[test_eval<4 & !is.na(ubcf_pred_values)]
TN_ubcf_knn=length(TN_ubcf_knn[TN_ubcf_knn<4])
FN_ubcf_knn=ubcf_pred_values[test_eval>3 & !is.na(ubcf_pred_values)]
FN_ubcf_knn=length(FN_ubcf_knn[FN_ubcf_knn<4])
FP_ubcf_knn=ubcf_pred_values[test_eval<4 & !is.na(ubcf_pred_values)]
FP_ubcf_knn=length(FP_ubcf_knn[FP_ubcf_knn>3])

ubcf_knn_conf_mat=matrix(c(TP_ubcf_knn,FP_ubcf_knn,FN_ubcf_knn,TN_ubcf_knn),2,2)
ubcf_knn_acc=sum(diag(ubcf_knn_conf_mat))/sum(ubcf_knn_conf_mat)
ubcf_knn_prec=ubcf_knn_conf_mat[1,1]/sum(ubcf_knn_conf_mat[,1])
ubcf_knn_recall=ubcf_knn_conf_mat[1,1]/sum(ubcf_knn_conf_mat[1,])
#ubcf threshold method
ubcf_thr_pred_values[ubcf_thr_pred_values==0]=NA

TP_ubcf_thr=ubcf_thr_pred_values[test_eval>3 & !is.na(ubcf_thr_pred_values)]
TP_ubcf_thr=length(TP_ubcf_thr[TP_ubcf_thr>3])
TN_ubcf_thr=ubcf_thr_pred_values[test_eval<4 & !is.na(ubcf_thr_pred_values)]
TN_ubcf_thr=length(TN_ubcf_thr[TN_ubcf_thr<4])
FN_ubcf_thr=ubcf_thr_pred_values[test_eval>3 & !is.na(ubcf_thr_pred_values)]
FN_ubcf_thr=length(FN_ubcf_thr[FN_ubcf_thr<4])
FP_ubcf_thr=ubcf_thr_pred_values[test_eval<4 & !is.na(ubcf_thr_pred_values)]
FP_ubcf_thr=length(FP_ubcf_thr[FP_ubcf_thr>3])

ubcf_thr_conf_mat=matrix(c(TP_ubcf_thr,FP_ubcf_thr,FN_ubcf_thr,TN_ubcf_thr),2,2)
ubcf_thr_acc=sum(diag(ubcf_thr_conf_mat))/sum(ubcf_thr_conf_mat)
ubcf_thr_prec=ubcf_thr_conf_mat[1,1]/sum(ubcf_thr_conf_mat[,1])
ubcf_thr_recall=ubcf_thr_conf_mat[1,1]/sum(ubcf_thr_conf_mat[1,])
#ibcf method
ibcf_pred_values[ibcf_pred_values==0]=NA

TP_ibcf=ibcf_pred_values[test_eval>3 & !is.na(ibcf_pred_values)]
TP_ibcf=length(TP_ibcf[TP_ibcf>3])
TN_ibcf=ibcf_pred_values[test_eval<4 & !is.na(ibcf_pred_values)]
TN_ibcf=length(TN_ibcf[TN_ibcf<4])
FN_ibcf=ibcf_pred_values[test_eval>3 & !is.na(ibcf_pred_values)]
FN_ibcf=length(FN_ibcf[FN_ibcf<4])
FP_ibcf=ibcf_pred_values[test_eval<4 & !is.na(ibcf_pred_values)]
FP_ibcf=length(FP_ibcf[FP_ibcf>3])

ibcf_conf_mat=matrix(c(TP_ibcf,FP_ibcf,FN_ibcf,TN_ibcf),2,2)
ibcf_acc=sum(diag(ibcf_conf_mat))/sum(ibcf_conf_mat)
ibcf_prec=ibcf_conf_mat[1,1]/sum(ibcf_conf_mat[,1])
ibcf_recall=ibcf_conf_mat[1,1]/sum(ibcf_conf_mat[1,])
#content based method
cb_pred_values[cb_pred_values==0]=NA

TP_cb=cb_pred_values[test_eval>3 & !is.na(cb_pred_values)]
TP_cb=length(TP_cb[TP_cb>3])
TN_cb=cb_pred_values[test_eval<4 & !is.na(cb_pred_values)]
TN_cb=length(TN_cb[TN_cb<4])
FN_cb=cb_pred_values[test_eval>3 & !is.na(cb_pred_values)]
FN_cb=length(FN_cb[FN_cb<4])
FP_cb=cb_pred_values[test_eval<4 & !is.na(cb_pred_values)]
FP_cb=length(FP_cb[FP_cb>3])

cb_conf_mat=matrix(c(TP_cb,FP_cb,FN_cb,TN_cb),2,2)
cb_acc=sum(diag(cb_conf_mat))/sum(cb_conf_mat)
cb_prec=cb_conf_mat[1,1]/sum(cb_conf_mat[,1])
cb_recall=cb_conf_mat[1,1]/sum(cb_conf_mat[1,])
#put them all together in a data frame
evaluation_results=data.frame('RMSE'=c(ubcf_RMSE,ubcf_thr_RMSE,ibcf_RMSE,cb_RMSE),
                              'Accuracy'=c(ubcf_knn_acc,ubcf_thr_acc,ibcf_acc,cb_acc),
                              'Precision'=c(ubcf_knn_prec,ubcf_thr_prec,ibcf_prec,cb_prec),
                              'Recall'=c(ubcf_knn_recall,ubcf_thr_recall,ibcf_recall,cb_recall))
row.names(evaluation_results)=c('UBCF/k-nn','UBCF/threshold','IBCF/threshold','Content Based')

