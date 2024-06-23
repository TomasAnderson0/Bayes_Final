load("ic_comp.RData")

ggsave("Imagenes/plot1.png", plot = posterior +
          labs(caption = "Distribuciones a posteriori de los parametros")  +  theme(plot.caption = element_text(size = 20,hjust = .5, color = "#555555")), width = 15, height = 5)
ggsave("Imagenes/plot2.png",plot = ggarrange(pps1, pps2)+
         labs(caption = expression(paste("Distribución a posteriori de", pi ," según los valores de la variable")))  +  theme(plot.caption = element_text(size = 20,hjust = .5, color = "#555555")), width = 15, height = 5)
ggsave("Imagenes/plot4.png",plot = cadenas + labs(caption = "Cadenas de MCMC obtenidas con HMC")  +  theme(plot.caption = element_text(hjust = .5, color = "#555555"), size = 50), width = 15, height = 5)


set.seed(123)
y0 = exp(rnorm(1000))
y1 = exp(rnorm(1000) + rnorm(1000))
y2 = exp(rnorm(1000) + rnorm(1000) + rnorm(1000))
y3 = exp(rnorm(1000) + rnorm(1000) + rnorm(1000) + rnorm(1000))

ye0 = y0/(y0+1)
ye1 = y1/(y1+1)
ye2 = y2/(y2+1)
ye3 = y3/(y3+1)


suma = data.frame(ye = c(quantile(ye0,.025),quantile(ye0,.975),quantile(ye1,.025),quantile(ye1,.975),quantile(ye2,.025),quantile(ye2,.975),quantile(ye3,.025),quantile(ye3,.975)), re= rep(0:3,each = 2) )

a = data.frame(x = c(median(ye0),median(ye1),median(ye2),median(ye3)))
a$cual = c("as", "S = Masculino\nI=1.5T\nR=GE", "S = Femenino\nI=3T\nR=GE","S = Femenino\nI=1.5T\nR=Philips")
plw = ggplot(a) + geom_bar(aes(x = cual, y = x), stat = "identity", fill = "#5aeeac") + 
  labs(x = "Nivel", y = expression(pi), caption = expression(paste("Distribución a priori de ", pi))) + geom_point(aes(x = cual, y = x), color = "#047e46")+ scale_y_continuous(limits = c(0, 1)) + geom_line(data = suma, aes(y = ye, x = re+1, group = re), color = "#047e46") + scale_x_discrete(labels = c(expression(beta[0]), expression(beta[0]+beta["S"]), expression(beta[0]+beta["I"]+beta["S"]), expression(beta[0]+beta["R"]+beta["I"]+beta["S"]))) + theme_minimal(base_size = 20) +  theme(plot.caption = element_text(hjust = .5, color = "#555555"))

ggsave("Imagenes/plot3.png",plot = plw, width = 15, height = 5)
ggsave("Imagenes/plot5.png",plot = ppc, width = 10, height = 5)





library(cowplot)

plotc1 = ggplot(cerebros) + geom_boxplot(aes(y = lh_subcx_hippocampus_volume), fill = "#168168") + theme_minimal() + theme(axis.ticks.x.bottom = element_blank(), axis.text.x.bottom = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + labs(title = expression(paste("VHI⠀", (cm^{3}))), y = "") + xlim(-.8,.8)

plotc2 = ggplot(cerebros) + geom_boxplot(aes(y = xh_general_etiv_volume), fill = "#168168") + theme_minimal() + theme(axis.ticks.x.bottom = element_blank(), axis.text.x.bottom = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + labs(title = expression(paste("VI⠀(100", cm^{3}, ")")), y = "") + xlim(-.8,.8)

plotc3 = ggplot(cerebros) + geom_boxplot(aes(y = lh_cortex_superiorfrontal_thickness), fill = "#168168") + theme_minimal() + theme(axis.ticks.x.bottom = element_blank(), axis.text.x.bottom = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + labs(title = expression(paste("ECSF⠀", (mm))), y = "") + xlim(-.8,.8)

plotc4 = ggplot(cerebros) + geom_boxplot(aes(y = lh_cortex_fusiform_volume), fill = "#168168")  + theme_minimal() + theme(axis.ticks.x.bottom = element_blank(), axis.text.x.bottom = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + labs(title = expression(paste("VCF⠀", (cm^{3}))), y = "")+ xlim(-.8,.8)

plotc5 = ggplot(cerebros) + geom_boxplot(aes(y = edad), fill = "#168168") + theme_minimal() + theme(axis.ticks.x.bottom = element_blank(), axis.text.x.bottom = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + labs(title = "Edad", y = "") + xlim(-.8,.8)

asdasd = ggarrange(plotc1, plotc2, plotc3, plotc4, plotc5) +
  labs( caption = "Distribución de las variables categoricas") + theme(plot.caption = element_text(hjust = .5, vjust = 2, color = "#555555"))





ggsave("Imagenes/plot6.png",plot = asdasd, width = 10, height = 5)







