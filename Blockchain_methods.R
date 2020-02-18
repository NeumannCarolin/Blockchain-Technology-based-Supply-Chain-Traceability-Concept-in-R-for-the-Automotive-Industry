
library("digest")



#Function that creates a hashed "block"
hash_block <- function(block){
  # input: block (list) one list of blockchain
  # return: block (list) hashed version of input
  
  block$new_hash <- digest(unlist(block, use.names=FALSE), "sha256")
  return(block)
}

### Simple Proof of Work Alogrithm
proof_of_work <- function(last_proof){
  # input: last_proof (number) proof of previous block in chain
  # return: proof (number) proof of new block which depends on previous one
  return(1)
  proof <- last_proof + 1
  
  # Increment the proof number until a number is found that is divisable by 99 and by the proof of the previous block
  while (!(proof %% 2 == 0 )){#& proof %% last_proof == 0 )){
    proof <- proof + 1
  }
  
  return(proof)
}


create_json_add <- function(id_t,producer,faulty,faulty_date,faulty_performance,produced_date,type)
{
  # input: ... (list) parameters used in standard factory table
  # return: ... vector of those parameters
  c(Type="Standard",
    ID = id_t,
    Producer = producer,
    Faulty = faulty,
    Faulty_date = faulty_date,
    Faulty_performance = faulty_performance,
    Produced_date = produced_date,
    Type=type,
    sql=(sprintf("INSERT INTO Part (ID, Producer,Faulty,Faulty_Date,Faulty_Performance,Produced_Date,Edited,Editor,Type) VALUES ('%s','%s','%s',Date('%s'),'%s',Date('%s'),0,'0','%s')",
                 id_t,producer,faulty,faulty_date,faulty_performance,produced_date,type)))
}

create_json_received <- function(id_t,producer,received_date,received_coordinates,faulty_received)
{
  # input: ... (list) parameters used in standard factory table
  # return: ... vector of those parameters
  c(Type="Received",
    ID = id_t,
    Producer = producer,
    received_date = received_date,
    received_coordinates = received_coordinates,
    faulty_received = faulty_received,
    sql=sprintf("UPDATE Transport SET Producer_Received='%s', Date_Received=Date('%s'), Coordinates_Received='%s', Faulty_Received='%s' where ID='%s'",
                producer,
                received_date,
                received_coordinates,
                faulty_received,
                id_t))
}

create_json_sent <- function(id_t,producer,sent_date,sent_coordinates)
{
  # input: ... (list) parameters used in standard factory table
  # return: ... vector of those parameters
  c(Type="Sent",
    ID = id_t,
    Producer = producer,
    sent_date = sent_date,
    sent_coordinates = sent_coordinates,
    sql=sprintf("INSERT INTO Transport (ID, Producer_Sent, Date_Sent, Coordinates_Sent) VALUES ('%s','%s',Date('%s'),'%s')",
                id_t,
                producer,
                sent_date,
                sent_coordinates))
}

create_json_edit <- function(id_t,producer,edit_name,edit_value)
{
  # input: ... (list) parameters used in standard factory table
  # return: ... vector of those parameters
  if(edit_name=='Faulty_Date' || edit_name=='Produced_Date'){ # value has to be Date
    c(Type="Edit",
      ID = id_t,
      Producer = producer,
      Edit_name = edit_name,
      Edit_value = edit_value,
      sql=sprintf("UPDATE Part SET %s=Date('%s'), Edited=1, Editor='%s' WHERE ID ='%s'",edit_name,edit_value,producer,id_t))
  }else if(edit_name=='Edited'){ # value has to be Int
    c(Type="Edit",
      ID = id_t,
      Producer = producer,
      Edit_name = edit_name,
      Edit_value = edit_value,
      sql=sprintf("UPDATE Part SET %s=%s, Edited=1, Editor='%s' WHERE ID ='%s'",edit_name,edit_value,producer,id_t))
  }else{ 
    # value has to be string
    c(Type="Edit",
      ID = id_t,
      Producer = producer,
      Edit_name = edit_name,
      Edit_value = edit_value,
      sql=sprintf("UPDATE Part SET %s='%s', Edited=1, Editor='%s' WHERE ID ='%s'",edit_name,edit_value,producer,id_t))
  }
}


create_json_build <- function(id_component,id_part,producer,build_date,build_coordinates,faulty,type)
{
  # input: ... (list) parameters used in standard factory table
  # return: ... vector of those parameters
  c(Type="Assembly",
    ID_Component = id_component,
    ID_Part=id_part,
    Producer = producer,
    Build_date = build_date,
    Build_coordinates = build_coordinates,
    Faulty = faulty,
    Type_Assembly=type,
    sql=sprintf("INSERT INTO Assembly VALUES ('%s','%s','%s',Date('%s'),'%s','%s','%s')",id_component, id_part ,producer ,build_date, build_coordinates,faulty,type))
}


gen_new_block <- function(previous_block,new_block_items){
  # input:
  #   previous_block (list)
  #   new_block_items (vector/json) new parameters which will be added into the BC
  # return:
  #   new_block_hashed (list) hashed block with new param.
  
  #Proof-of-Work
  new_proof <- proof_of_work(previous_block$proof)
  
  #Create new Block
  new_block <- append(list(index = previous_block$index + 1,
                           timestamp = Sys.time(),
                           data = paste0("this is block ", previous_block$index +1),
                           previous_hash = previous_block$new_hash,
                           proof = new_proof),new_block_items)
  #Hash the new Block
  new_block_hashed <- hash_block(new_block)
  
  return(new_block_hashed)
}

create_block <- function(previous_block,new_block_items,blockchain){
  # input:
  #   previous_block (list)
  #   new_block_items (vecotr/json) 
  #   blockchain (list of lists) 
  # return:
  #   blockchain (list of lists) with one more block appended
  
  # creates new block with all param.
  block_to_add <- gen_new_block(previous_block,new_block_items) 
  
  # appends it onto the BC
  blockchain[length(blockchain)+1] <- list(block_to_add)
  
  return(blockchain)
}

get_previous_block <- function(blockchain){
  # input: blockchain (list of lists)
  # return: block (list) only the last block
  
  return(blockchain[length(blockchain)][[1]])
}

# blockchain to html----
print_blockchain_text <- function(b,blockchain){
  l = length(blockchain)
  i = 1
  str2 = ''
  for(i in 1:l){
    str2 <- paste(str2,"<b>Last Block :</b> <br/>", block_to_text(blockchain[[i]]),'<br/>','<br/>')
    i <- i+1
  }
  return(HTML(paste(str2, sep = '<br/>')))
}

print_blockchain_text_first <- function(b,blockchain){
  l = length(blockchain)
  i = 1
  str2 = ''
  for(i in 1:l){
    str2 <- paste(str2,"<b>First Block :</b> <br/>", block_to_text(blockchain[[i]]),'<br/>','<br/>')
    i <- i+1
  }
  return(HTML(paste(str2, sep = '<br/>')))
}

block_to_text <- function(block){
  b = do.call("cbind",block)
  col = colnames(b)
  l = length(col)
  i = 1
  str = ''
  for(i in 1:l){
    str <- paste(str,'<font size="2" color="blue"> ',col[i], " :</font>       ", b[i],'<br/>')
    i <- i+1
  }
  return(HTML(paste(str)))
}


# Methods to create new blocks:----
# create first block
start_blockchain <- function(){
  # input: NONE
  # return: blockchain (list of list) with one element, genesis block
  
  # Define Genesis Block (index 1 and arbitrary previous hash)
  block_genesis <-  list(index = 1,
                         timestamp = str(Sys.time()),
                         data = "Genesis Block",
                         previous_hash = "0",
                         proof = 1)
  blockchain <- list(block_genesis)
  
  return(blockchain)
}

LoadToEnvironment <- function(RData, env = new.env()){
  load(RData, env)
  return(env) 
}

# create more blocks----
#blockchain <- start_blockchain()
#blockchain <- create_block(get_previous_block(blockchain),create_json_factory("11-215-2154-102","215","2154",0,NA,0,"01-01-3333"),blockchain)

new_database <- function(file,new){
  if(new == "new_by_button"){
    con <<- DBI::dbConnect(RSQLite::SQLite(), sprintf("./DB/New_%s",file))
  }else{
    con <<- DBI::dbConnect(RSQLite::SQLite(), sprintf("%s",file))}
  if(new == "new"||length(dbListTables(con))==0){
    try(dbSendQuery(conn=con,
                    "CREATE TABLE Part
                    (ID Text,
                    Producer Text,
                    Faulty Text,
                    Faulty_Date DATE,
                    Faulty_Performance Text,
                    Produced_Date DATE,
                    Edited Integer,
                    Editor Text,
                    Type Text)"))
    
    try(dbSendQuery(conn=con,
                    "CREATE TABLE Transport
                    (ID Text,
                    Producer_Sent Text,
                    Producer_Received Text,
                    Date_Sent DATE,
                    Date_Received DATE,
                    Coordinates_Sent Text,
                    Coordinates_Received Text,
                    Faulty_Received Integer)"))
    
    try(dbSendQuery(conn=con,
                    "CREATE TABLE Assembly
                    (ID_Assembly Text,
                    ID_Part Text,
                    Producer Text,
                    Build_Date DATE, 
                    Build_Coordinates Text,
                    Faulty Text,
                    Type_Assembly Text)"))}
  }

from_BC_into_DB <- function(con, public_blockchain){
  i=2
  blockchain <<- public_blockchain
  for(i in 2:length(public_blockchain)){
    dbSendQuery(con,public_blockchain[[i]]$sql)
  } 
  return(blockchain)
}

synch_blockchain <- function(con, public_blockchain, priv_blockchain){
  i=length(priv_blockchain)
  for(i in length(priv_blockchain):length(public_blockchain)){
    dbSendQuery(con,public_blockchain[[i]]$sql)
    session$sendCustomMessage(type = 'testmessage',
                              message = public_blockchain[[i]]$sql)
    #message(public_blockchain[[i]]$sql)
  } 
  priv_blockchain = public_blockchain
  return(priv_blockchain)
  
}

