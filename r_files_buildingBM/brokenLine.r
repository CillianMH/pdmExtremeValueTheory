# Broken Line Process
# 23-04-2015
# Killian Martin--Horgassan

brokenLine = function(u) {
	
	Int_part <- floor(u)
	Float_part <- u-floor(u)
	
	# Y_t = S_floor(t) + (t-floor(t))*Epsilon_(floor(t)+1)
	Epsilons <- Epsilon_is(Int_part + 1)
	#S <- randomWalk(Int_part, Epsilons)
	
	#output <- S[Int_part] + Float_part*Epsilons[Int_part + 1]
	output <- cumsum(Epsilons)
	
	return(output)
}