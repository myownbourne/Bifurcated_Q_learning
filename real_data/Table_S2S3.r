read_data = read.csv("r_add.csv")
new_array = array(data = read_data$value, dim = c(max(read_data$row), max(read_data$col), max(read_data$matrix)))
sim_result = new_array


sim_result_med = apply(sim_result, c(1, 2), mean)
#sim_result_med = apply(sim_result[, , 1:100], c(1, 2), mean)

print(round(sim_result_med[,1:4],4))
print(round(sim_result_med[,5:10],4))
print(round(sim_result_med[,11:18],4))
print(round(sim_result_med[,19:24],4))
print(round(sim_result_med[,25:32],4))


