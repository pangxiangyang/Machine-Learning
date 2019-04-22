library(readxl)
accidents <- read_excel( "C:\\Users\\PXY\\Desktop\\Vehicle&Casualties.xlsx")
View( accidents)

# Plot all vehicles types & total deaths against Year
plot(TotalDeaths ~ Year, data=accidents, main="Number of incidents over the years", ylab ="Number of cases", xlab = "", ylim =c(600, 140000), type ="o", col="black" )
lines( accidents$`Year`, accidents$Lorry, col = "red", type="o")
lines( accidents$`Year`, accidents$Motorcycle, col = "blue", type="o")
lines( accidents$`Year`, accidents$Motocar, col = "green", type="o")
lines( accidents$`Year`, accidents$Van, col = "purple", type="o")
lines( accidents$`Year`, accidents$Bus, col = "orange", type="o")
lines( accidents$`Year`, accidents$`Four Wheel Drive`, col = "darkgreen", type="o")
lines( accidents$`Year`, accidents$Taxi, col = "yellow", type="o")
lines( accidents$`Year`, accidents$Bicycle, col = "aquamarine", type="o")
legend(2008, 100000, legend=c("TotalDeaths","Lorry","Motorcycle","Motorcar","Van","Bus","Four Wheel Drive", "Taxi","Bicycle" ),
       col=c("black","red", "blue", "green", "purple","Orange","darkgreen","yellow","aquamarine"), 
       lty=1:1, cex=0.8)

# Correlation testing
correlationResult <- cor( accidents )
correlationResult <- format(correlationResult, digits = 3)

# Plot Motorcycle vs total deaths
plot(TotalDeaths ~ Motorcycle, data=accidents, main="Motorcycle versus TotalDeaths", col = "red" )

# Plot Motocar vs total deaths
plot(TotalDeaths ~ Motocar, data=accidents, main="Motocar versus TotalDeaths", col = "red" )

# Build simple linear regression model ( Motocycle )
lmmodel = lm(TotalDeaths ~ Motorcycle, data = accidents )
summary(lmmodel)
abline(lmmodel, col='blue')

# Create new dataframe with new columns ( Motocycle )
mototestmodel <- accidents
mototestmodel$moto3 <- mototestmodel$Motorcycle^3
mototestmodel$moto2 <- mototestmodel$Motorcycle^2
mototestmodel$moto4 <- mototestmodel$Motorcycle^4
mototestmodel$moto5 <- mototestmodel$Motorcycle^5
 
# Build non-linear regression model ( Motocycle )
lmmodel2=lm(TotalDeaths~ Motorcycle + moto3 + moto2 + moto4 + moto5, data = mototestmodel)
summary( lmmodel2 )

# Predict & plot prediction results ( Motocycle )
plot(TotalDeaths ~ Motorcycle, data=accidents, ylim=c( 6400, 7250 ),main="Motorcycle versus TotalDeaths", col = "red" )
testvalue <- seq(105000,144000, 400)
predicteddeaths <- predict( lmmodel2, list( Motorcycle = testvalue, moto5 = testvalue^5, moto4 = testvalue^4, moto3 =testvalue^3, moto2 =  testvalue^2 ) )
lines(testvalue, predicteddeaths, col = "blue", lwd =1 )

#########################################################################################

# Build simple linear regression model ( Motocar )
lmmodelcar = lm(TotalDeaths ~ Motocar, data = accidents )
summary(lmmodelcar)
abline(lmmodelcar, col='blue')

# Create new dataframe with new columns ( Motocar )
mototestmodel$car3 <- mototestmodel$Motocar^3
mototestmodel$car2 <- mototestmodel$Motocar^2

# Build non-linear regression model ( Motocar )
lmmodelcar2=lm(TotalDeaths~ Motocar + car3 + car2 , data = mototestmodel)
summary( lmmodelcar2 )

# Predict & plot prediction results ( Motocar )
plot(TotalDeaths ~ Motocar, data=accidents, main="Motocar versus TotalDeaths", col = "red" )
testvaluecar <- seq(400000,700000, 500)
predicteddeathscar <- predict( lmmodelcar2, list( Motocar = testvaluecar,  car3 =testvaluecar^3, car2 =  testvaluecar^2 ) )
lines(testvaluecar, predicteddeathscar, col = "blue", lwd =1 )

# Aggregate the numbers based on number of wheels
mototestmodel$twowheels <- mototestmodel$Motorcycle + mototestmodel$Bicycle 
mototestmodel$fourwheels <- mototestmodel$Motocar + mototestmodel$`Four Wheel Drive` + mototestmodel$Taxi
mototestmodel$otherwheels <- mototestmodel$Total - mototestmodel$twowheels - mototestmodel$fourwheels

# Aggregate the numbers based on private/ public transport
mototestmodel$private <- mototestmodel$Motocar + mototestmodel$Motorcycle + mototestmodel$Bicycle
mototestmodel$public <- mototestmodel$Van + mototestmodel$Taxi + mototestmodel$Bus

# Plot scatter plots of private vs public, and number of wheels
plot(TotalDeaths ~ public, data=mototestmodel,  main="Number of incidents over the years",  col = "red" )
plot(TotalDeaths ~ private, data=mototestmodel,  main="Number of incidents over the years",  col = "red" )
plot(TotalDeaths ~ twowheels, data=mototestmodel,  main="Number of incidents over the years",  col = "red" )
plot(TotalDeaths ~ fourwheels, data=mototestmodel,  main="Number of incidents over the years",  col = "red" )
plot(TotalDeaths ~ otherwheels, data=mototestmodel,  main="Number of incidents over the years",  col = "red" )

# Correlation testing
with(mototestmodel, cor.test(`TotalDeaths`, `public`))
with(mototestmodel, cor.test(`TotalDeaths`, `private`))
with(mototestmodel, cor.test(`TotalDeaths`, `twowheels`))
with(mototestmodel, cor.test(`TotalDeaths`, `publicwheels`))
with(mototestmodel, cor.test(`TotalDeaths`, `otherwheels`))

# Build non-linear regression model ( private )
mototestmodel$private2 <- mototestmodel$private^2
mototestmodel$private3 <- mototestmodel$private^3
privatemodel <- lm( `TotalDeaths`~`private` + private2 + private3 , data = mototestmodel)
summary(privatemodel)

