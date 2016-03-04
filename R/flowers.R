flowers <- datasets::iris3
dim(flowers) <- c(150,4)
colnames(flowers) <- c("slength","swidth", "plength", "pwidth")
set.seed(421)
nmiss <- c(3,1,2,0)
for(j in c(1,2,3)) {
    nas <- sample(1:50,nmiss[j])+50
    flowers[nas,j] <- NA
}
