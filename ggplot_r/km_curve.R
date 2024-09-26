#' Function to draw Kaplan-Meier curve.
#' 
#' depending on library:
#' survfit, ggplot2, ggfortify, ggpubr, ggsurvfit
#' 
#' @param ${1:Model} ${2:Survfit format Kaplan-Meier Model: Model <- survfit(Surv(Time, Event) ~ Var, data = DF).}
#' @param ${1:Title} ${2:The figure title.}  
#' @param ${1:X_Lab} ${2:The label of x (time) axis.}
#' @param ${1:Max_T} ${2:Max of x (time) axis to display in figure. }
km_curve <- function(Model, Title, X_Lab, File, Max_T) {

p <- ggsurvfit(Model) +
  add_confidence_interval() +
  add_risktable(size = 6) +
  scale_ggsurvfit() + 
  ggtitle(Title) + 
  xlab(X_Lab) + 
  ylab("Probability") +
  theme_bw() + 
  xlim(0, Max_T) +
  scale_x_continuous(breaks = round(seq(0, Max_T, by = 600),1)) +
#  annotate("text", x=c(12 * 30, 36 * 30, 120 * 30) ,y=c(0, 0, 0), hjust=-.2, vjust=0, label=c("12M", "36M", "120M"), size = 8) +  
#  geom_vline(xintercept = c(12 * 30, 36 * 30, 120 * 30), linetype="dotted", 
#             linewidth = 1.2) +
  theme(legend.title = element_text(size=24, face="bold"),
        legend.text = element_text(size = 20),
        legend.position = c(0.9, 0.82),
        plot.title = element_text(size=26, hjust = .5),
        axis.title.x = element_text(size=20, face="bold"),
        axis.title.y = element_text(size=20, face="bold"),
        axis.text = element_text(size = 18)
  ) 

p 

}