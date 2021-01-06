#' @title Tamanho da Amostra - 2 Fatores n observações  
#' @encoding UTF-8
#' @name testeFelipe
#'
#' @description Função que calcula o Erro Tipo II e o Poder do Teste em um experimento com 2 fatores e n observações.
#'
#' @param alpha Erro Tipo I esperado 
#' @param a Tamanho do Fator 1  
#' @param b Tamanho do Fator 2
#' @param ate Cálcular o erro Tipo II de n=2 até o valor especificado neste parâmetro  
#' @param betas Vetor contendo o valor dos Betas (ou distância)
#' @param sigma Desvio-padrão
#'
#' @details
#' Parâmetro de não-centralidade: 
#' \deqn{\phi^2=\frac{an\sum_{j=1}^b\beta_i^2}{\sigma^2}}
#' Erro tipo II: 
#' \deqn{\beta(\phi^2)=P(F\leq F_{crítico}|F_{(df1=b-1,\;df2=ab(n-1),\;npc=\phi^2)})}  
#' Poder do teste: 
#' \deqn{1-\beta(\phi^2)}  
#' 
#' @return Matrix contendo  \code{n}, \code{a}, \code{b}, \code{df1}, \code{df2}, \code{f_crit}, \code{phi_2}, \code{errotip2} e \code{poder_do_teste}.
#'
#' @author Felipe da Rocha Ferreira
#'
#' @references MONTGOMERY, Douglas C. Design and Analysis of Experiments. 8. ed
#'
#' @seealso \code{\link[stats]{qf}}, \code{\link[stats]{pf}}
#'
#' @examples
#' 
#' ##Page 202 - Design and Analysis of Experiments 8th by DOUGLAS C. MONTGOMERY: 
#' testeFelipe(alpha=0.05,a=50,b=5,ate=50,betas=c(-20,20),sigma=25)
#'
#' @importFrom stats pf qf
#'
#' @export
testeFelipe<-function(alpha,a,b,ate,betas,sigma){
  Erro2_2fatores_Fator2<-function(){
    n<-c();f_crit<-c();phi_2<-c();errotipo2<-c();poder<-c()
    df1<-c();df2<-c()
    for(i in 2:ate){
      df1[i]<-b-1
      df2[i]<-a*b*(i-1)
      f_crit[i]<-qf(1-alpha,df1[i],df2[i])
      phi_2[i]<-(a*i*sum(betas^2))/(sigma^2)
      n[i]<-i
    }
    dados<-data.frame(n,a,b,df1,df2,f_crit,phi_2)
  }
  resultado<-Erro2_2fatores_Fator2()[-1,]
  resultado$errotip2<-pf(resultado$f_crit,resultado$df1,resultado$df2,
                         resultado$phi_2)*100
  resultado$poder_do_teste<-100-resultado$errotip2
  print(resultado)
}