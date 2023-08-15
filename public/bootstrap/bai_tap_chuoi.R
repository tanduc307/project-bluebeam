# ### Viết chương trình cho phép người dùng:

# - Nhập vào một chuỗi. In chuỗi.

# - Tạo chuỗi mới từ chuỗi đã có trong đó tất cả các ký tự đã nhập ở trên đều được in hoa.

# - Tách chuỗi đã nhập ở đầu bài thành danh sách các phần tử từ.

# Nhập vào một từ. In từ.

# - Tìm xem từ vừa nhập có là phần tử nào trong danh sách phần tử trên hay không? 

# Nếu tìm thấy thì thông báo là "Found!!!" ngược lại thì thông báo là "Not found!!!"

# - Nhập vào một từ cần tìm (find) trong chuỗi và một từ mới sẽ thay thế (replace). 
# Thay thế tất cả các từ tìm (find) trong chuỗi bằng từ thay thế (replace)

function_1 <- function(input_data){
               
            # a_1 <- toupper(input_data)  
            
            toupper(input_data)  -> a_1
            
            return(a_1)   
               
}

## test function

vi_du <- c("làm bài tập Giữa kỳ")

function_1(vi_du)

####

function_2 <- function(input_data_1){
               
             a_2 <- strsplit(input_data_1, split = "")
             
             return(a_2)
               
} 

## test function

vi_du <- c("làm bài tập Giữa kỳ")

function_2(input_data_1 = vi_du)

################

key_word <- "bài"

# Nhập vào một từ. In từ.

# - Tìm xem từ vừa nhập có là phần tử nào trong danh sách phần tử trên hay không? 

vi_du <- c("làm bài tập Giữa kỳ")

vector_bat_ky <- c("hoàn thành bài tập aaa")

function_3 <- function(tu_khoa, chuoi_1){
               
               
               grep(pattern = tu_khoa,
                    x = chuoi_1,
                    value = TRUE,
                    ignore.case = TRUE,
                    fixed = FALSE) -> yes_1
               
               return(yes_1)
               
               
}

function_3(chuoi_1 = vi_du, tu_khoa = key_word) 

function_3(chuoi_1 = vector_bat_ky, tu_khoa = key_word) 

function_3(chuoi_1 = c(vi_du, vector_bat_ky), tu_khoa = key_word) 

#################################

function_3 <- function(tu_khoa, chuoi_1){
               
               grep(pattern = tu_khoa,
                    x = chuoi_1,
                    value = TRUE,
                    ignore.case = TRUE,
                    fixed = FALSE) -> yes_1
               
               if(length(yes_1) >= 1) {
                              print("Found")
                              return(yes_1)
                              
               } else { 
                              print("Not Found")
                              return(yes_1)}
               
               
}

##### 

vector_d <- "làm toán ở nhà"

function_3("toán", vector_d)
##########


# - Nhập vào một từ cần tìm (find) trong chuỗi và một từ mới sẽ thay thế (replace). 

function_sub <- function(find, replace, chuoi_1) {
               
               
              sub(pattern = find,
                   replacement = replace,
                   x = chuoi_1) -> yes_2
               
               return(yes_2)
               
}

vector_d <- "làm toán ở nhà"

vector_e <- "làm toán ở nhà, học toán ở trường"

function_sub(find = "toán", replace = "bài tập", chuoi_1 = vector_d)

function_sub(find = "toán", replace = "bài tập", chuoi_1 = vector_e)


# Thay thế tất cả các từ tìm (find) trong chuỗi bằng từ thay thế (replace)


function_gsub <- function(find, replace, chuoi_1) {
               
               
               gsub(pattern = find,
                    replacement = replace,
                    x = chuoi_1) -> yes_2
               
               return(yes_2)
               
}


function_gsub(find = "toán", replace = "bài tập", chuoi_1 = vector_d)

function_gsub(find = "toán", replace = "bài tập", chuoi_1 = vector_e)





























































