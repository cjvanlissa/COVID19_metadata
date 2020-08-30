get_signs <- function(pdps){
  sapply(pdps, function(x){
    thisvar <- x$data
    out <- table(sign(c(-1, 0, 1, diff(thisvar$preds))))-1
    if(any(out[c(1,3)] == 0) & !all(out[c(1,3)] == 0)){
      out <- c(out, c("Negative monotonous", "Positive monotonous")[which(!out[c(1,3)] == 0)])
    } else {
      if(!out[1] == out[3]){
        out <- c(out, c("Mostly negative", "Mostly positive")[(out[1] < out[3])+1])
      } else {
        out <- c(out, "Other")
      }
    }
    dif_amount <- tapply(c(0,0,0, diff(thisvar$preds)), c(-1, 0, 1, sign(diff(thisvar$preds))), sum)
    if(any(dif_amount[c(1,3)] == 0) & !all(dif_amount[c(1,3)] == 0)){
      dif_amount <- c(dif_amount, c("Negative monotonous", "Positive monotonous")[which(!dif_amount[c(1,3)] == 0)])
    } else {
      if(abs(dif_amount[3]+.00001) / abs(dif_amount[1]+.00001) > 3){
        dif_amount <- c(dif_amount, "Mostly positive")
      } else {
        if(abs(dif_amount[1]+.00001) / abs(dif_amount[3]+.00001) > 3){
          dif_amount <- c(dif_amount, "Mostly negative")
        } else {
          if(!((abs(dif_amount[3]+.00001) / abs(dif_amount[1]+.00001) > 3) | (abs(dif_amount[1]+.00001) / abs(dif_amount[3]+.00001) > 3))){
            dif_amount <- c(dif_amount, "Other")
          }
        }
      }
    }
    c(names(thisvar)[1], out, dif_amount)
  })
}
