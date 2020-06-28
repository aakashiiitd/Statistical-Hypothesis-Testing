#Question 1

dataset<-c("Ai","Akemi","Akiko","Ayumi","Chiaki","Chie","Eiko","Eri",
         "Eriko","Fumiko","Harumi","Hit-omi","Hiroko","Hi- roko",
         "Hidemi","Hisako","Hinako","Izumi","Izumi","Junko","Junko",
         "Kana","Kanako","Kanayo","Kayo","Kayoko","Kazumi","Keiko",
         "Keiko","Kei","Kumi","Kumiko","Kyoko","Kyoko","Madoka",
         "Maho","Mai","Maiko","Maki","Miki","Miki","Mikiko", 
         "Mina","Minako","Miyako","Momoko","Nana","Naoko","Naoko",
         "Naoko","Noriko","Rieko","Rika","Rika","Rumiko","Rei",
         "Reiko","Reiko","Sachiko","Sachiko","Sachiyo","Saki",
         "Sayaka","Sayoko","Sayuri","Seiko","Shiho","Shizuka",
         "Sumiko","Takako","Takako","Tomoe","Tomoe","Tomoko","Touko",
         "Yasuko","Yasuko","Yasuyo","Yoko","Yoko","Yoko","Yoshiko",
         "Yoshiko","Yoshiko","Yuka","Yuki","Yuki","Yukiko","Yuko","Yuko")

checkKO <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

countKO=0
countNotKO=0

for (names in dataset){
  if(checkKO(names,2)=="ko"){ 
    countKO=countKO+1
  }
  else{
    countNotKO=countNotKO+1
  }
}

#From Question, We have
a=0.05 #Alpha
p=0.7
q=0.3  #q=1-p

p2=countKO/length(dataset)

#Z-Statistics
z=(p2-p)/sqrt((p*q)/length(dataset))

#Value from z critical table for alpha
z2=-1.645

#Z P-value
p3=pnorm(-abs(z))

cat('Names ending with KO: ',countKO)
cat('Names not ending with KO: ',countNotKO)
cat('Z-satistics: ',z)
cat('Z Critical Value: ',z2)
cat('Z P-value: ',p3)