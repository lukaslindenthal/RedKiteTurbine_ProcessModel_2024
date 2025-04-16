Version5 - changes

# new data structure
	- Red kite data is saved in data.frame list
	- Red kite metric: total abundance, nests, juv, new_juv, new_nests, natural dead, killed when moving, killed by turbine building
	- total output for all timesteps "kites_abund“

# movement: 
	- if 3 kites are at the same coordinate -> it stays at the old loc
	- if max attempts were reached and there are still >2 kites at one location, set lonely_age to 0
