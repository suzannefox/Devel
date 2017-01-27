library(RODBC);
channel <- odbcConnect("SQL Server", uid="sa", pwd="coffeecup");
p <- sqlQuery(channel, "
              SELECT * FROM tbl_Cases
              ");
close(channel);


with (p, table(strat_Brand_launch, Year))

(p[,c("Target_Gender")])

FileName <- ""
testdata <- p
myData <- myContent("Header", testdata, FileName)

for(i in names(myData)) {
  myContent(myData,i,FileName)
}
