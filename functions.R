#McGrath Eeuations:
# This is the green equation on Dr. McGrath's 2019 spreadsheet.
cole_ewel <- function(d,h){
  biomass <- 1.631+.017*(d^2)*h
  carbon <- biomass * 0.5
  CO2equ <- carbon * 3.6663
  CO2equ_tons <- CO2equ *0.001102
  return(CO2equ_tons)
}

#This is the orange equation on Dr. McGrath's spreadsheet.
TFTF <- function(d,h){
  Abovebiomass <- 0.25*((d/2.54)^2)*(h*3.28)
  Mass <- Abovebiomass/2.205
  ABBiomass <- Mass*1.2
  carbon <- ABBiomass * 0.5
  CO2equ <- carbon *3.6663
  CO2equ_tons <- CO2equ *0.001102
  return(CO2equ_tons)
}

#-------------------------------------------------------------------------------------------
#Equations that we found
#This is our found equation for Mango. The final answer is given in tons.
Sharma_Mango <- function(d){
  Abovebiomass <- 34.4703 - 8.067*(d) + 0.6589*(d^2)
  BelowGroundBiomass <- Abovebiomass*1.2
  TotalBiomass <- BelowGroundBiomass + Abovebiomass
  carbon <- TotalBiomass * 0.5
  CO2equ_kg <- carbon *3.6663
  CO2equ_tons <- CO2equ_kg *0.001102
  return(CO2equ_tons)
}

#This is the equation for the Mahogany tree.

Dickert_Mahogany <- function(d,h){
  Abovebiomass <- 0.09029*((d^2)*h)^(0.684)
  BlowGround <- Abovebiomass *1.2
  AGABG <- Abovebiomass + BlowGround
  carbon <- AGABG * 0.5
  CO2equ_kg <- carbon *3.6663
  CO2equ_tons <- CO2equ_kg *0.001102
  return(CO2equ_tons)
}


# This is the cedrela tree equation from the Cole and Ewel paper.
Cole_Cedrela <- function(d,h){
  Abovebiomass <- 0.0448*((d^2)*h)^(0.4879)
  BlowGround <- Abovebiomass *1.2
  AGABG <- Abovebiomass + BlowGround
  carbon <- AGABG * 0.5
  CO2equ_kg <- carbon *3.6663
  CO2equ_tons <- CO2equ_kg *0.001102
  return(CO2equ_tons)
}


#This equations came from the Mexico paper.
Acosta_Coffee <- function(d){
  Abovebiomass <- exp(-0.66)*((d)^1.37)
  carbonTree <- Abovebiomass *0.5
  BelowBiomass <- carbonTree*1.2
  AGABGBiomass <- BelowBiomass + carbonTree
  CO2equ_kg <- AGABGBiomass *3.6663
  CO2equ_tons <- CO2equ_kg *0.001102
  return(CO2equ_tons)
}



#This equations is from the Chave paper and gives a general equation for carbon.
Chave_General <- function(d,h){
  Abovebiomass <- (0.0673)*((.42)*(d^2)*(h))^0.976
  carbonTree <- Abovebiomass *0.5
  BelowBiomass <- carbonTree*1.2
  AGABGBiomass <- BelowBiomass + carbonTree
  CO2equ_kg <- AGABGBiomass *3.6663
  CO2equ_tons <- CO2equ_kg *0.001102
  return(CO2equ_tons)
}
