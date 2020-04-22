#separating genres
genres=as.data.frame(movies$Genres)
genres2=as.data.frame(tstrsplit(genres[,1],'\\|'),type.convert=T,stringsAsFactors = FALSE)
colnames(genres2)=1:6
genres_list=unique(unlist(genres2))
is.na(genres_list)
genres_list=genres_list[!is.na(genres_list)]
genres_list=sort(genres_list)


#create genres table
genre_matrix=matrix(0,3776,18)
colnames(genre_matrix)=genres_list
genre_matrix[1,]=genres_list

#for loop to give the value 1 if genre corresponds in movie's row(genre2 matrix)
for (j in 1:ncol(genre_matrix)){
  for (i in 1:nrow(genres2)){
    genre_matrix[i+1,j]=ifelse(TRUE%in%(genre_matrix[1,j]==genres2[i,]),1,0)
  }
}

genre_matrix=as.data.frame(genre_matrix,stringsAsFactors = FALSE)
for (i in 1:ncol(genre_matrix)){
  genre_matrix[,i]=as.integer(genre_matrix[,i])
}

genre_matrix=genre_matrix[-1,]

#update movies data table with the separated genres
#genre_matrix=as.data.table(genre_matrix[-1,])
movies_upd=cbind(movies[,c('MovieID','Title','Year')],genre_matrix)

subset(movies_upd,Action==1&Comedy==1&Year%in%1950:1990)$Title
subset(movies_upd,Documentary==1)$Title

###We have to update the genre matrix with the missing movies
genre_rownames=movies$MovieID
genre_bind=cbind(genre_rownames,genres2)
miss_mov=matrix(NA,177,7)
miss_mov[,1]=setdiff(1:3952,movies$MovieID)
miss_mov=as.data.frame(miss_mov)
colnames(miss_mov)=colnames(genre_bind)
genres2_upd=rbind(genre_bind,miss_mov)
genres2_upd=genres2_upd[order(genres2_upd[,1]),]
rownames(genres2_upd)=genres2_upd[,1]
genres2_upd=genres2_upd[,-1]

###After that we follow the same procedure to create the updated genre matrix
genre_matrix_upd=matrix(0,3953,18)
colnames(genre_matrix_upd)=genres_list
genre_matrix_upd[1,]=genres_list

#for loop to give the value 1 if genre corresponds in movie's row(genre2_upd matrix)
for (j in 1:ncol(genre_matrix_upd)){
  for (i in 1:nrow(genres2_upd)){
    genre_matrix_upd[i+1,j]=ifelse(TRUE%in%(genre_matrix_upd[1,j]==genres2_upd[i,]),1,0)
  }
}
genre_matrix_upd=as.data.frame(genre_matrix_upd,stringsAsFactors = FALSE)
for (i in 1:ncol(genre_matrix_upd)){
  genre_matrix_upd[,i]=as.integer(genre_matrix_upd[,i])
}
genre_matrix_upd=genre_matrix_upd[-1,]
########Attempt on content-based filtering#########

#building of Profile and Item vectors

#profile_table:rows -> movieID
#              cols -> UserID
users_profile=ratings[,c('UserID','MovieID','Rating')]
users_profile=as.data.table(users_profile)
profile_table=as.data.table(dcast(users_profile,MovieID~UserID,value.var = 'Rating',fill=0, na.rm=FALSE))

#some movies are not rated so the correspondance between rows and movies is not right
#so we are gonna fill the missing ids with 0 rating

profile_matrix=as.matrix(profile_table)
missing_movie_id=setdiff(1:3952,profile_matrix[,'MovieID'])
length(missing_movie_id)
patch=matrix(0,246,6041)
patch[,1]=missing_movie_id
profile_matrix_2=rbind(profile_matrix,patch)
profile_matrix_2=profile_matrix_2[order(profile_matrix_2[,1]),]
unique(as.data.table(profile_matrix_2[50:52,1:10]))#check
profile_table=as.data.table(profile_matrix_2)
str(profile_table)
profile_table=profile_table[,-1] #final
profile_matrix_2=profile_matrix_2[,-1] #final
rownames(profile_matrix_2)=c(1:3952)# need it to define rownames
#profile_matrix_2[50:52,1:10]
#profile_table[50:52,1:10]
ncol(profile_matrix_2)
remove(profile_matrix)

#normalizing genres matrix
g_matrix=as.matrix(genre_matrix_upd)
genre_matrix_norm=matrix(0,3952,18)
sum_row=NULL
for (i in 1:nrow(g_matrix)){
  sum_row[i]=0
  sum_row[i]=sum(g_matrix[i,])
}
genre_matrix_norm=g_matrix/sqrt(sum_row) #normalized item vectors
genre_matrix_norm[is.nan(genre_matrix_norm)]=0
#creating profile vectors
prof_vec_matrix=matrix(0,6040,18)
prof_vec_matrix=t(profile_matrix_2[,])%*%(genre_matrix_norm[,])
norm_rows=NULL
for (i in 1:nrow(prof_vec_matrix)){
  norm_rows[i]=prof_vec_matrix[i,]%*%prof_vec_matrix[i,]
}
prof_vec_matrix_norm=prof_vec_matrix/sqrt(norm_rows) #normalized profile vectors

###calculation of inverted genre frequency in movies
#DF=NULL
#DF[1:18]=0 #Document Frequency
#for (i in 1:ncol(g_matrix)){
#  DF[i]=sum(g_matrix[,i])
#}
#IDF=log10(3775/DF) #Inverted Document Frequency

###calculate the weighted value of each movie
#weighted_values=matrix(0,3775,18)
#for (i in 1:nrow(genre_matrix_norm)){
#  weighted_values[i,]=genre_matrix_norm[i,]*IDF
#}

###Prediction of user's ratings or rating recommendation
recom=prof_vec_matrix_norm[,]%*%t(genre_matrix_norm[,]) #recommend without taking weight into account
#recom_movies=prof_vec_matrix_norm[,]%*%t(weighted_values[,]) # recommend by taking weight into account
    
###Attemp with binary prof_matrix
bin_prof_matrix=matrix(0,3952,6040)
for (i in 1:nrow(profile_matrix_2)){
  for (j in 1:ncol(profile_matrix_2)){
    if (profile_matrix_2[i,j]>3){
      bin_prof_matrix[i,j]=1
    }
    else if (profile_matrix_2[i,j]>0){
      bin_prof_matrix[i,j]=-1
    }
  }
}
bin_prof_matrix[1:1000,1:3] #check
prof_vec=t(bin_prof_matrix[,])%*%genre_matrix_norm[,]
###Predictions using binarized prof_matrix
recom_2=prof_vec[,]%*%t(genre_matrix_norm[,])
#recom_movies_2=prof_vec[,]%*%t(weighted_values[,])  #weighted 

###check similarity between the recommenders(by using cosine similarity on the recommenders)
recom[1,]%*%recom_2[1,]/(sqrt(recom[1,]%*%recom[1,])*sqrt(recom_2[1,]%*%recom_2[1,]))
#recom_movies[1,]%*%recom_movies_2[1,]/(sqrt(recom_movies[1,]%*%recom_movies[1,])*sqrt(recom_movies_2[1,]%*%recom_movies_2[1,]))
#recom[1,]%*%recom_movies[1,]/(sqrt(recom[1,]%*%recom[1,])*sqrt(recom_movies[1,]%*%recom_movies[1,]))
#recom_movies_2[1,]%*%recom_2[1,]/(sqrt(recom_movies_2[1,]%*%recom_movies_2[1,])*sqrt(recom_2[1,]%*%recom_2[1,]))
#both binary and normal ratings gives similar results

# lets recommend 10 movies to user 1
colnames(recom)=1:3952
rownames(recom)=1:6040
user1_recom=data.table(recom_score=recom[1,],movieID=1:3952)
user1_recom=user1_recom[order(recom_score, decreasing = TRUE),]
top_10=user1_recom[1:10,.(recom_score,movieID)]
movies_upd[MovieID%in%top_10[,movieID],.(MovieID,Title)]

# recommendation based on binary matrix
colnames(recom_2)=1:3952
rownames(recom_2)=1:6040
user1_recom_2=data.table(recom_score=recom_2[1,],movieID=1:3952)
user1_recom_2=user1_recom_2[order(recom_score, decreasing = TRUE),]
top_10_2=user1_recom_2[1:10,.(recom_score,movieID)]
movies_upd[MovieID%in%top_10[,movieID],.(Title)]
#you can compare the two