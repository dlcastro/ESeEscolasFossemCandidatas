##Projeto: E se Escolas fossem pessoas concorrendo no SISU?
##Autor: Daniel Lopes de Castro
##Contato: https://decastro.me
##Lattes:http://lattes.cnpq.br/1893509400555503
##Twitter: @euDLCastro
##GitHub: dlcastro
####################################

require(tidyverse)
require(ggplot2)
require(readxl)
require(ggthemes)


#Áreas do ENEM: para identificar as Sheets do Excel
AREAS <- list("RED","MT","LC","CN","CH")
#Informação de Escolas que são interessantes
PERFIL <- c("CO_ENTIDADE","TP_DEPENDENCIA","NUM_ALUN_CENSO2015","IND_SOCIOECONOMICO","TX_ABANDONO")

#Função de importar os dados das notas médias das Escolas no ENEM 2015
agreg <- function(x){

    for (i in x) {

        ENEM <-   read_excel(path = "ENEM/Planilhas_Enem_2015.xlsx", sheet = i, col_names = T) %>%
          select(CO_ENTIDADE,MEDIA_ESCOLA,MEDIA_30_MELHORES_ALUNOS)


      names(ENEM)[names(ENEM) == "MEDIA_ESCOLA"] <- paste0(i, "_MEDIA_ESCOLA")
      names(ENEM)[names(ENEM) == "MEDIA_30_MELHORES_ALUNOS"] <- paste0(i, "_30_MELHORES_ALUNOS")


       #assign(paste0("ENEM_",i),ENEM, envir = .GlobalEnv)

       PERFIL_ENEM <<- left_join(PERFIL_ENEM,ENEM,by="CO_ENTIDADE")
       ENEM_ESCOLAS <<- PERFIL_ENEM

    }
}

###########IMPORTAR BANCOS DE DADOS BASE############
PERFIL_ENEM <- read_excel("ENEM/Planilhas_Enem_2015.xlsx", sheet = "RED",col_names = T) %>%
    select(all_of(PERFIL))

#Gerará o ENEM_ESCOLAS (que no fim será o mesmo que o Perfil_Enem porque sim)
lapply(AREAS, agreg)
ENEM_ESCOLAS <- ENEM_ESCOLAS %>%
    mutate(ANO="2015.1",
           NIVEL_SOCIO = case_when(IND_SOCIOECONOMICO == "Muito Baixo" ~ 1,
                                    IND_SOCIOECONOMICO == "Baixo" ~ 2,
                                    IND_SOCIOECONOMICO == "Médio Baixo" ~ 3,
                                    IND_SOCIOECONOMICO == "Médio" ~ 4,
                                    IND_SOCIOECONOMICO == "Médio Alto" ~ 5,
                                    IND_SOCIOECONOMICO == "Alto" ~ 6,
                                    IND_SOCIOECONOMICO == "Muito alto" ~ 7))

ENEM_CORTE <- read_excel("ENEM/ENEM_CORTE.xlsx", sheet = "2015-1",col_names = T) %>%
    mutate(CAMPUS = trimws(toupper(CAMPUS)),
           IES = trimws(toupper(IES)),
           CURSO = trimws(toupper(CURSO)),
           TURNO = trimws(toupper(TURNO)),
           FORMACAO = trimws(toupper(FORMACAO)))


ENEM_PESO <- read_excel("ENEM/ENEM_PESO.xlsx", sheet = "1-2015", col_names = T) %>%
    mutate(CAMPUS = trimws(toupper(CAMPUS)),
           IES = trimws(toupper(IES)),
           CURSO = trimws(toupper(CURSO)),
           TURNO = trimws(toupper(TURNO)),
           FORMACAO = trimws(toupper(FORMACAO)))
######################################################


#Tratar os dados de nota do ENEM.
#IMPORTANTE: Ainda que não seja matematicamente correto, irei gerar uma média aritmética das notas de cada curso,
#isso gerará a nota de corte "nacional" daquele curso. Do ponto de vista prático, isso não é problemático, uma vez
#que a grande maioria dos cursos fazer média aritmetica e porque a nota de corte foi o último aluno que passou,
#não tendo muito sentido teórico nisso.
#Outra coisa que pode ajudar nesse "erro matemático", é que irei calcular as notas das escolas utilizando os
#pesos de TENDÊNCIA de cada curso no cenário nacional. Uma média aritmética dos pesos indicara essa tendência por curso.

ENEM_BRASIL <-  left_join(ENEM_PESO, ENEM_CORTE, by =c("IES","CAMPUS","CURSO","TURNO","FORMACAO"))%>%
    select(CURSO, FORMACAO,MODALIDADE, CORTE,starts_with("PESO"))



ENEM_BRASIL_TRATADO <- ENEM_BRASIL %>%
    group_by(CURSO, FORMACAO, MODALIDADE) %>%
    filter(CORTE!=0) %>%
    summarize(PESO_HUMANAS = round(mean(PESO_HUMANAS),digits = 0),
              PESO_LINGUAGEM = round(mean(PESO_LINGUAGEM),digits = 0),
              PESO_NATUREZA = round(mean(PESO_NATUREZA),digits = 0),
              PESO_MATEMATICA = round(mean(PESO_MATEMATICA),digits = 0),
              PESO_REDACAO = round(mean(PESO_REDACAO),digits = 0),
              NOTA_CORTE = round(mean(CORTE), digits = 2)) %>%
    filter(MODALIDADE=="Ampla Concorrência" | MODALIDADE == "Candidatos que, independentemente da renda (art. 14, II, Portaria Normativa nº 18/2012), tenham cursado integralmente o ensino médio em escolas públicas (Lei nº 12.711/2012)." |
               MODALIDADE == "Candidatos com renda familiar bruta per capita igual ou inferior a 1,5 salário mínimo que tenham cursado integralmente o ensino médio em escolas públicas (Lei nº 12.711/2012).") %>%
    mutate(ANO="2015.1",
           SOMA_PESOS = PESO_LINGUAGEM + PESO_HUMANAS + PESO_NATUREZA + PESO_MATEMATICA + PESO_REDACAO)


CURSOS_ESCOLHIDOS <- c("MEDICINA", "ENGENHARIA CIVIL", "DIREITO", "ADMINISTRAÇÃO", "ENFERMAGEM") #adicionarei Pedagogia no Filter para remover Bacharelado

CURSOS_SELECIONADOS <- ENEM_BRASIL_TRATADO %>%
    filter(CURSO %in% CURSOS_ESCOLHIDOS | (CURSO == "PEDAGOGIA" & FORMACAO =="LICENCIATURA")) %>%
    full_join(ENEM_ESCOLAS, by="ANO")





##Gerar notas por escola utilizando os Pesos calculados por curso
##

ENEM_WIDE<- CURSOS_SELECIONADOS %>%
    mutate(NOTA_MEDIA_ESCOLA = (((RED_MEDIA_ESCOLA*PESO_REDACAO) + (MT_MEDIA_ESCOLA*PESO_MATEMATICA) + (LC_MEDIA_ESCOLA*PESO_LINGUAGEM) +
                                   (CN_MEDIA_ESCOLA*PESO_NATUREZA) + (CH_MEDIA_ESCOLA*PESO_HUMANAS))/SOMA_PESOS),
           NOTA_30_MELHORES_ESCOLA = (((RED_30_MELHORES_ALUNOS*PESO_REDACAO) + (MT_30_MELHORES_ALUNOS*PESO_MATEMATICA) + (LC_30_MELHORES_ALUNOS*PESO_LINGUAGEM) +
                                           (CN_30_MELHORES_ALUNOS*PESO_NATUREZA) + (CH_30_MELHORES_ALUNOS*PESO_HUMANAS))/SOMA_PESOS),
           APROVADO_MEDIA = ifelse(NOTA_MEDIA_ESCOLA >= NOTA_CORTE,1,0),
           APROVADO_30 = ifelse(NOTA_30_MELHORES_ESCOLA >= NOTA_CORTE,1,0))





###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################

NOTAS_CORTE_AMPLA <- ENEM_WIDE %>%
    group_by(CURSO, MODALIDADE) %>%
    summarize(NOTA_CORTE= mean(NOTA_CORTE)) %>%
    mutate(NOME_MODALIDADE = case_when(MODALIDADE=="Ampla Concorrência" ~ "Ampla Concorrência",
                                       MODALIDADE=="Candidatos que, independentemente da renda (art. 14, II, Portaria Normativa nº 18/2012), tenham cursado integralmente o ensino médio em escolas públicas (Lei nº 12.711/2012)." ~ "Cota Esc. Pública",
                                       MODALIDADE=="Candidatos com renda familiar bruta per capita igual ou inferior a 1,5 salário mínimo que tenham cursado integralmente o ensino médio em escolas públicas (Lei nº 12.711/2012)." ~ "Cota Esc. Pública + Renda"))


NOTAS_CORTE_COTAS_PUB <- ENEM_WIDE %>%
    filter(MODALIDADE=="Candidatos que, independentemente da renda (art. 14, II, Portaria Normativa nº 18/2012), tenham cursado integralmente o ensino médio em escolas públicas (Lei nº 12.711/2012).") %>%
    group_by(CURSO) %>%
    summarize(NOTA_CORTE= mean(NOTA_CORTE))

NOTAS_CORTE_COTAS_RENDA <- ENEM_WIDE %>%
    filter(MODALIDADE=="Candidatos com renda familiar bruta per capita igual ou inferior a 1,5 salário mínimo que tenham cursado integralmente o ensino médio em escolas públicas (Lei nº 12.711/2012).") %>%
    group_by(CURSO) %>%
    summarize(NOTA_CORTE= mean(NOTA_CORTE))




## TOP 30 - Todas as Escols
ENEM_WIDE %>%
    filter(MODALIDADE=="Ampla Concorrência" & TP_DEPENDENCIA!="Privada" ) %>%
    ggplot() +
    geom_jitter(aes(x=as.numeric(NIVEL_SOCIO), y=NOTA_30_MELHORES_ESCOLA, color = TP_DEPENDENCIA)) +
    geom_hline(data = NOTAS_CORTE_AMPLA,aes(yintercept=NOTA_CORTE, linetype=NOME_MODALIDADE), color = "black", size =1)+
    scale_x_discrete(name ="Indicador socioeconômico",
                     limits=c("|Muito Baixo|","|Baixo|","|Médio Baixo|","|Médio|","|Médio Alto|","|Alto|","|Muito Alto|"))+
    facet_wrap(~CURSO, scales="free") +
    theme_bw()+
    ylim(400, 850)+
    guides(colour = guide_legend(override.aes = list(size=5))) +
    ggtitle("Nota média no SISU dos 30 alunos mais bem pontuados por Escola (2015.1)")+
    ylab("Nota ENEM/SISU") +
    labs(color = "Escola:",
         linetype = "Nota de corte:",
         caption = " Projeto: E se Escolas fossem pessoas concorrendo no SISU? \n Autor: Daniel Lopes de Castro (@euDLCastro | https://decastro.me | GitHub:dlcastro) \n Fonte: ENEM e SISU 2015 (INEP) | Produzido em: Julho/2020")+
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          axis.title = element_text(face = "bold", size = 15),
          axis.text.x = element_text(face="bold", color="#993333",
                                     size=6, angle=0,vjust = 1.1),
          legend.position = "top",
          legend.text = element_text(size=10, face = "bold"),
          strip.text = element_text(face="bold"),
          plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0))


ggsave("Resultado/30_AMPLA_CONCORRENCIA.png", width = 33, height = 16.5, units = "cm", dpi = 300)


##Top 30 - Cotas Escola Pública

ENEM_WIDE %>%
    filter(MODALIDADE=="Candidatos que, independentemente da renda (art. 14, II, Portaria Normativa nº 18/2012), tenham cursado integralmente o ensino médio em escolas públicas (Lei nº 12.711/2012)." & TP_DEPENDENCIA!="Privada" ) %>%
    ggplot() +
    geom_jitter(aes(x=as.numeric(NIVEL_SOCIO), y=NOTA_30_MELHORES_ESCOLA, color = TP_DEPENDENCIA)) +
    geom_smooth(aes(x=as.numeric(NIVEL_SOCIO), y=NOTA_30_MELHORES_ESCOLA),method = "lm", se = FALSE, color="black")+
    geom_hline(data = NOTAS_CORTE_COTAS_PUB,aes(yintercept=NOTA_CORTE), color = "black", linetype="dashed", size =1)+
    geom_text(data = NOTAS_CORTE_COTAS_PUB,aes( 0, NOTA_CORTE, label = "Nota de corte (Cota E.P.)", vjust = -0.4,hjust = -0.01 ), size = 3)+
    geom_text(data = NOTAS_CORTE_COTAS_PUB,aes( 0, NOTA_CORTE, label = "Não aprovados", vjust = 2,hjust = -0.01 ),fontface = "bold", size = 3)+
    geom_text(data = NOTAS_CORTE_COTAS_PUB,aes( 0, NOTA_CORTE, label = "Aprovados", vjust = -2,hjust = -0.01 ),fontface = "bold", size = 3)+
    scale_x_discrete(name ="Indicador socioeconômico",
                     limits=c("|Muito Baixo|","|Baixo|","|Médio Baixo|","|Médio|","|Médio Alto|","|Alto|","|Muito Alto|"))+
    facet_wrap(~CURSO, scales="free") +
    theme_bw()+
    ylim(400, 850)+
    guides(colour = guide_legend(override.aes = list(size=5))) +
    ggtitle("Nota média no SISU dos 30 alunos mais bem classificados por Escola",
            subtitle = "Disputa por vagas de cotas Escola Pública sem discriminação de renda ou outra característica")+
    ylab("Nota ENEM/SISU") +
    labs(color = "Escola:",
         caption = " Projeto: E se Escolas fossem pessoas concorrendo no SISU? \n Autor: Daniel Lopes de Castro (@euDLCastro | https://decastro.me | GitHub:dlcastro) \n Fonte: ENEM e SISU 2015 (INEP) | Produzido em: Julho/2020")+
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          axis.title = element_text(face = "bold", size = 15),
          axis.text.x = element_text(face="bold", color="#993333",
                                    size=6, angle=0,vjust = 1.1),
          legend.position = "top",
          legend.text = element_text(size=10, face = "bold"),
          strip.text = element_text(face="bold"),
          plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0))



ggsave("Resultado/30_COTAS_PUBLICAS.png", width = 33, height = 16.5, units = "cm", dpi = 300)



ENEM_WIDE %>%
    filter(MODALIDADE=="Candidatos com renda familiar bruta per capita igual ou inferior a 1,5 salário mínimo que tenham cursado integralmente o ensino médio em escolas públicas (Lei nº 12.711/2012)." & TP_DEPENDENCIA!="Privada" ) %>%
    ggplot() +
    geom_jitter(aes(x=as.numeric(NIVEL_SOCIO), y=NOTA_30_MELHORES_ESCOLA, color = TP_DEPENDENCIA)) +
    geom_smooth(aes(x=as.numeric(NIVEL_SOCIO), y=NOTA_30_MELHORES_ESCOLA),method = "lm", se = FALSE, color="black")+
    geom_hline(data = NOTAS_CORTE_COTAS_RENDA,aes(yintercept=NOTA_CORTE), color = "black", linetype="dashed", size =1)+
    geom_text(data = NOTAS_CORTE_COTAS_RENDA,aes( 0, NOTA_CORTE, label = "Nota de corte (Cota Renda)", vjust = -0.4,hjust = -0.01 ), size = 3)+
    geom_text(data = NOTAS_CORTE_COTAS_RENDA,aes( 0, NOTA_CORTE, label = "Não aprovados", vjust = 2,hjust = -0.01 ),fontface = "bold", size = 3)+
    geom_text(data = NOTAS_CORTE_COTAS_RENDA,aes( 0, NOTA_CORTE, label = "Aprovados", vjust = -2,hjust = -0.01 ),fontface = "bold", size = 3)+
    scale_x_discrete(name ="Indicador socioeconômico",
                     limits=c("|Muito Baixo|","|Baixo|","|Médio Baixo|","|Médio|","|Médio Alto|","|Alto|","|Muito Alto|"))+
    facet_wrap(~CURSO, scales="free") +
    theme_bw()+
    ylim(400, 850)+
    guides(colour = guide_legend(override.aes = list(size=5))) +
    ggtitle("Nota média no SISU dos 30 alunos mais bem classificados por Escola",
            subtitle = "Disputa por vagas de cotas Escola Pública com renda familiar bruta per capita igual ou inferior a 1,5 salário mínimo")+
    ylab("Nota ENEM/SISU") +
    labs(color = "Escola:",
         caption = " Projeto: E se Escolas fossem pessoas concorrendo no SISU? \n Autor: Daniel Lopes de Castro (@euDLCastro | https://decastro.me | GitHub:dlcastro) \n Fonte: ENEM e SISU 2015 (INEP) | Produzido em: Julho/2020")+
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          axis.title = element_text(face = "bold", size = 15),
          axis.text.x = element_text(face="bold", color="#993333",
                                     size=6, angle=0,vjust = 1.1),
          legend.position = "top",
          legend.text = element_text(size=10, face = "bold"),
          strip.text = element_text(face="bold"),
          plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0))



ggsave("Resultado/30_COTAS_RENDA.png", width = 33, height = 16.5, units = "cm", dpi = 300)





