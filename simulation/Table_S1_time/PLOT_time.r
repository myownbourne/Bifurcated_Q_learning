setting = 6

if(setting==1){read_data = read.csv("result_S11_time.csv")} #model 1
if(setting==3){read_data = read.csv("result_S11n_time.csv")} #model 3
if(setting==2){read_data = read.csv("result_S12_time.csv")} #model 2
if(setting==7){read_data = read.csv("result_S13_rho3_time.csv")} #model 7
if(setting==5){read_data = read.csv("result_S13_time.csv")} #model 5
if(setting==4){read_data = read.csv("result_S14_time.csv")} #model 4
if(setting==6){read_data = read.csv("result_S17_k1_time.csv")} #model 6
if(setting==8){read_data = read.csv("result_S18_time.csv")} #model 8
if(setting==9){read_data = read.csv("result_S19_d100_n300_time.csv")} #model 9
if(setting==10){read_data = read.csv("result_S15_time.csv")} # model 10
if(setting==11){read_data = read.csv("result_S16_time.csv")} # model 11

sim_result  = array(data = read_data$value, dim = c(max(read_data$row), max(read_data$col), max(read_data$matrix)))
sim_result2 = apply(sim_result, c(1, 2), mean)
sim_result3 = apply(sim_result, c(1, 2), sd)

print(round(sim_result2,2))
print(round(sim_result3,2))