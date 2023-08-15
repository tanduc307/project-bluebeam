# https://stackoverflow.com/questions/11007178/creating-a-prompt-answer-system-to-input-data-into-r

fun <- function(){
               x <- readline("What is the value of x?")  
               y <- readline("What is the value of y?")
               t <- readline("What are the T values?")
               v <- readline("What are the V values?")
               
               x <- as.numeric(unlist(strsplit(x, ",")))
               y <- as.numeric(unlist(strsplit(y, ",")))
               t <- as.numeric(unlist(strsplit(t, ",")))
               v <- as.numeric(unlist(strsplit(v, ",")))
               
               out1 <- x + y
               out2 <- t + v
               
               return(list(out1, out2))
               
}


# run fun() trong console để xem interactive


#############

menu(c("Yes", "No"), title="Do you want this?")

##############


fun <-function(){
               input<-NULL
               x<-NULL
               input<-menu(c("lowercase", "UPPERCASE"),title="What type of letters do want to assign to x?") + 1
               if (input == 1){
                              x<-NULL
                              message('Nothing assigned to x')
               }
               if (input == 2){
                              x<-letters
                              message('x is lowercase!')
               }
               if (input == 3){
                              x<-letters
                              message("x is UPPERCASE!")
               }
}
