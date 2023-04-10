
#plancks:6.62607004 × 10-34 m2 kg / s


#Viir sensitivity = the nominal minimum detection in-band radiance for the DNB is 3 × 10???5 W·m???2·sr???1

h<-6.62607004e-34 
c<-3e8
#wav =
wav<-490 *10e-9
# e = jouls per photon
e<-(h*c)/wav
# p = photon production per second
p<-10^4 
#JpI = joules per individual per second
JpI<-p*e


# dino densities from article:
#"Some observations on dinoflagellate population density during a bloom  in a California reservoir"
#d= cell per m^3 by multiplying cell count per milliter: 
d<-200*10e6
#prod: prodction efficany ( cellaur)
prod<-.1
#at<- attneation(absorbtion+sacttering)
at<-.05
stre<-(180/pi)^2
hemi<-2*pi*stre
#divded by two for upward light
light_per_cubic_meter<-(d*prod*at)/2
lm<-light_per_cubic_meter
sesn<-lm/hemi
print(paste((round(sesn, 2)), "watts * sr^-1 * m^2", sep =" "))
