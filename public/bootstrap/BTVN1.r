#Bài tập tuần 1 

# Nhập vào một chuỗi từ người dùng
input_string <- readline("Nhập vào một chuỗi: ")

# In chuỗi đã nhập
#cách 1:
print(paste("thông tin vừa nhập", input_string))
#cách 2:
cat("thông tin vừa nhập", input_string,"\n")

# Nhập vào một ký tự và in ký tự
input_char <- readline("Nhập vào một ký tự: ")
cat("Ký tự vừa nhập: ", input_char, "\n")

# Tách chuỗi thành danh sách các từ
word_list <- strsplit(input_string, " ")[[1]]
cat("Danh sách các từ trong chuỗi: ", word_list, "\n")
----

# Tạo chuỗi mới từ chuỗi đã có với tất cả các ký tự in hoa
new_string <- toupper(input_string)
cat("Chuỗi mới với tất cả ký tự in hoa: ", new_string, "\n")

# Tách chuỗi thành danh sách các từ
word_list <- strsplit(input_string, " ")[[1]]
cat("Danh sách các từ trong chuỗi: ", word_list, "\n")

# Nhập vào một từ và in từ
input_word <- readline("Nhập vào một từ: ")
cat("Từ vừa nhập: ", input_word, "\n")

# Kiểm tra xem từ vừa nhập có trong danh sách hay không
if (input_word %in% word_list) {
  cat("Found!!!\n")
} else {
  cat("Not found!!!\n")
}

# Nhập vào từ cần tìm và từ thay thế
find_word <- readline("Nhập vào từ cần tìm: ")
replace_word <- readline("Nhập vào từ thay thế: ")

# Thay thế từ cần tìm trong chuỗi và in kết quả
replaced_string <- gsub(find_word, replace_word, input_string)
cat("Chuỗi sau khi thay thế: ", replaced_string, "\n")




ok <- function(x, y){
               x+y
}


#################################################

nhap_du_lieu <- function(input_data){
               
               
               paste(input_data, 0) -> w_1
               
               
               return(w_1)
               
               }

c() 

yes_1 <- c("a", "b", "c")

yes_1

yes_2 <- "abc"

yes_2

yes_3 <- ("abc")

yes_3

mean(1:10) -> happy


(mean(1:10) -> happy)



# yes <- ("a", "b", "c")




nhap_du_lieu("ky hieu bat ky") -> result_1


nhap_du_lieu(yes) -> result_2



###################################

1:10

mean(1:10) * 100

print(mean(1:10))

print(1:10)


print.default(mean(1:10))


print

methods(print)


print.default(1:10)


day_so <- c(1, 5, 7, 9, 20)

mean(day_so) * 100

function_1 <- function(input_day_so){
               
               
               mean(input_day_so) * 100 -> ok
               
               return(ok)
               
}

######################

## Bài tập số 1:
### Viết chương trình cho phép người dùng:

# - Nhập vào một chuỗi. In chuỗi. "string"


# - Nhập vào một ký tự. In ký tự. "character"



# - Tạo chuỗi mới từ chuỗi đã có trong đó tất cả các ký tự đã nhập ở trên đều được in hoa.

a <- "happy aa dd DA"

b <- "nguyen_tan_duc"

c <- "nguyentanduc"

d <- "nguyen tan duc"

toupper(a)

# - Tách chuỗi đã nhập ở đầu bài thành danh sách các phần tử từ.

strsplit(c, split = " ")

strsplit(d, split = "")

# - Nhập vào một từ. In từ.


# - Tìm xem từ vừa nhập có là phần tử nào trong danh sách phần tử trên hay không? Nếu tìm thấy thì thông báo là "Found!!!" ngược lại thì thông báo là "Not found!!!"

a <- c("happy aa dd DA", "kasjdkldraasd", "sajkd DA DA", "kasjdkldDADAr", "Da", "da da", "DADArjdkldraas")

b <- "DA"

grep_1 <- grep(pattern = "DA", 
               fixed = FALSE, 
               ignore.case = FALSE, 
               x = a, 
               value = TRUE)

grep_2 <- grep(pattern = "DA", 
               fixed = FALSE, 
               ignore.case = TRUE, 
               x = a, 
               value = TRUE)


grep_3 <- grep(pattern = "(D|d)(A|a)", 
               fixed = TRUE, 
               ignore.case = FALSE, 
               x = a, 
               value = TRUE)


grep_4 <- grep(pattern = "DA|Da|dA|ad", 
               fixed = FALSE, 
               ignore.case = FALSE, 
               x = a, 
               value = TRUE)


grep_2 <- grep(pattern = "DA", 
               fixed = FALSE, 
               ignore.case = TRUE, 
               x = a, 
               value = TRUE)



gsub()






grep(pattern = "DA", fixed = TRUE, x = a, value = TRUE)


"regular expression"

# - Nhập vào một từ cần tìm (find) trong chuỗi và một từ mới sẽ thay thế (replace). Thay thế tất cả các từ tìm (find) trong chuỗi bằng từ thay thế (replace)



# gsub(pattern = "DA", 
#      
#      , x = a, value = TRUE)
# 
# ?gsub



?chartr

x <- "MiXeD cAsE 123"
chartr(old = "iXs", new = "why", x)

x <- "MiXeD cAsE 123"
chartr("iXs", "why", x)

#########################

x_1 <- "MixseD case 123"
chartr(old = "ixs", new = "WHY", x_1)

grep()
gsub(pattern = "ixs",
     replacement = "why",
     x_1)


append("a", "b") -> c1
c
c("a", "b") -> p
p















































