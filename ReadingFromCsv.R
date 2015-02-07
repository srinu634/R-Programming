mydata = read.csv("C:\\Users\\redhawk\\Desktop\\Thesis\\R programming basics\\listeria.csv")
mydata
head(mydata)
########################
ls() 
character(0)


########################

name = scan(what="character")
age = scan()
hgt = scan()
wgt = scan()
race = scan(what="character")
year = scan(what="character")
SAT = scan()
my.data = data.frame(name, age, hgt, wgt, race, year, SAT)
