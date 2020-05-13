library(ggplot2)

plot_fore_obs <- function(obs_data, test_data, fore_data, horizon){
    
    obs_seq <- 1:length(obs_data)
    test_fore_seq <- (length(obs_data) + 1):(length(obs_data) + horizon)
    
    df1 <- data.frame(time = obs_seq, y = obs_data, type = "observations")
    
    df2 <- data.frame(time = test_fore_seq, y = test_data, type = "test")
 
    df3 <- data.frame(time = test_fore_seq, y = fore_data , type = "forecast")
    
    df <- rbind(df1, df2, df3)
    
    ggplot(df, aes(x = time, y = y, color = type)) + 
        geom_line() + 
        scale_colour_manual(values=c(observations='black', test = 'blue' , forecast='red'))#, guide = T)

}
