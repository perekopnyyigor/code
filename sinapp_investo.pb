Structure base
  name.s
  volume.f
EndStructure 

Structure zp
 dolzhnost.s
   zarplata.f
   ipn.f
   opv.f
   mzp.f
   vakansii.i
   s_nalog.f
   s_othisleniya.f
EndStructure 

Structure zatraty
   name.s
   price.f
   kolvo.f
   edizm.s
   srok.i
 EndStructure 
 
 Structure vlo
   name.s
   volume.i
   procent.f
   EndStructure
 
#dolzhnost=602
#zarplata=604
#vakansii=606
#ipn=621
#opv=623
#mzp=625
#s_nalog=627
#s_othisleniya=629

#add_zp=62
#summa=64
#redact_zp=610
 
#name=702
#price=704
#kolvo=706
#edizm=727
#add_zatrat=72
#summazatrat=74
#button_table_point=947

#del_zp=611
#del_zp_button=612

#redact_nomer_zarplat=607
#redact_nazvanie_zarplat=608
#redact_razmer_zarplat=609
#redact_kol_vo_vakansii=619
;711 712
Enumeration 
  #text_kap_zatrat_name=1000
  #gadget_kap_zatrat_name
  #text_kap_zatrat_price
  #gadget_kap_zatrat_price
  #text_kap_zatrat_volume
  #gadget_kap_zatrat_volume
  #text_kap_zatrat_edizm
  #gadget_kap_zatrat_edizm
  #add_kap_zatrat
  #list_kap_zatat
  #summa_kap_zatrat
  #text_summa_kap_zatrat
  #red_kap_number
  #red_kap_name
  #red_kap_price
  #red_kap_volume
  #red_kap_edizm
  #red_kap_sum
  #del_kap_number
  #text_red_kap
  #text_red_kap_number
  #text_red_kap_name
  #text_red_kap_price
  #text_red_kap_volume
  #text_red_kap_edizm
  #text_red_kap_sum
  #but_red_kap
  #but_del_kap
  #spin_srok
  #text_srok
  
    #text_post_zatrat_name
  #gadget_post_zatrat_name
  #text_post_zatrat_price
  #gadget_post_zatrat_price
  #text_post_zatrat_volume
  #gadget_post_zatrat_volume
  #text_post_zatrat_edizm
  #gadget_post_zatrat_edizm
  #add_post_zatrat
  #list_post_zatat
  #summa_post_zatrat
  #text_summa_post_zatrat
  #red_post_number
  #red_post_name
  #red_post_price
  #red_post_volume
  #red_post_edizm
  #red_post_sum
  #del_post_number
  #text_red_post
  #text_red_post_number
  #text_red_post_name
  #text_red_post_price
  #text_red_post_volume
  #text_red_post_edizm
  #text_red_post_sum
  #frame_post_zatrat
  #container_post_zatrat
  #but_red_post
  #but_del_post
   
  
  #iznos_frame
  #iznos_container
  #button_add_iznos
  #list_iznos
  
  
  #vlozheniya_frame
  #vlozheniya_container
  #button_add_vlozheniya
  #button_vlozheniya
  #list_vlozheniya
  #text_vlozheniya_name
  #gadget_vlozheniya_name
  #text_vlozheniya_price
  #gadget_vlozheniya_price
  #raschet
  #rezhim
  #pokazatel
  #canvas
  #button_del_vlozh
EndEnumeration

Enumeration
  #post=1
  #iznos
  #kap
  #zp
  #perem
  #invest
EndEnumeration

Enumeration menu
 #menu_kap =100
 #menu_perem
 #menu_zp
 #menu_vlozh
 #menu_post
 #menu_iznos
 #menu_table_tochka
 #menu_tochka
 #menu_table_ocup
 #menu_ocup
 #menu_koef
EndEnumeration

Enumeration window
  #window_kap=100
  #window_vlozh
  #window_zp
  #window_post
  #window_iznos
  #window_perem
  #window_table_tochka
  #window_tochka
  #window_table_ocup
  #window_ocup
  #window_koef
  #main_window
  
  EndEnumeration
 
Global Quit, Dim sum.f(20), Dim zatrat.base(20,100),Dim count(20),Dim zp.zp(100),p.f,Dim discont.f(100),flagdisc,Dim vol.f(100),flagvol,directory.s, Dim flag(10),optim_price, Dim zatraty.zatraty(100)
Global Dim toc_hka.s(100), Dim table_ocupaimost.s(100), Dim ocu_paimost.s(100), Dim vlozhenie.vlo(100), count_vlozhenie=2, Dim p.f(20), Dim poin_t.f(20),Dim koef_doc.s(20),optim_point
Global Dim post.zatraty(100), Dim perem.zatraty(100), Dim kap.zatraty(100), rezhim=2, nalog_dohod.f=0.03,  nalog_prib.f=0.2,rezhim,tochka_bezubit.f,ocup.s,glob_vol,glob_desc.f,glob_p
count(1)=2

directory.s;="d:\save.snp"

Declare iznos(t)
Declare panel_table_tochka()
Declare panel_tochka()
Declare panel_table_ocup()
Declare panel_ocup()
Declare raschet_vlozheniya(t)
Declare panel()
Declare check_open_winow()

Procedure iznos(t)
   
  sum_likvid=0
    sum_ga=0
  For i=1 To count(#kap)
    
    likvid=kap(i)\price*0.1*kap(i)\kolvo
    godovaya_amartizazzia=(kap(i)\price*0.9)*kap(i)\kolvo/kap(i)\srok
    
    sum_likvid=sum_likvid+likvid
    sum_ga=sum_ga+godovaya_amartizazzia
    If t=1
    AddGadgetItem(#list_iznos, i,kap(i)\name+Chr(10)+FormatNumber(kap(i)\price*kap(i)\kolvo)+Chr(10)+StrF(kap(i)\srok)+Chr(10)+FormatNumber(likvid)+Chr(10)+FormatNumber(godovaya_amartizazzia,0))
    EndIf
  Next
  If t =1
  AddGadgetItem(#list_iznos, i+1,"")
  AddGadgetItem(#list_iznos, i+2,"Итого"+Chr(10)+FormatNumber(sum(#kap))+Chr(10)+""+Chr(10)+FormatNumber(sum_likvid)+Chr(10)+FormatNumber(sum_ga))
  EndIf
  sum(#iznos)=sum_ga
  
   post(2)\name="амартизация"
   post(2)\price=sum(#iznos)
   post(2)\kolvo=1
   post(2)\edizm=""
   
  EndProcedure

Procedure dobavit_zp()
   count(#zp)=count(#zp)+1
   contn=count(#zp)
   
   zp(count(#zp))\dolzhnost=GetGadgetText(#dolzhnost)
   zp(count(#zp))\zarplata=ValF(GetGadgetText(#zarplata))
   ;zp(count(4))\ipn=ValF(GetGadgetText(#ipn))
   ;zp(count(#zp))\opv=ValF(GetGadgetText(#opv))
   ;zp(count(4))\mzp=ValF(GetGadgetText(#mzp))
   zp(count(#zp))\vakansii=ValF(GetGadgetText(#vakansii))
   
   ;zp(count(#zp))\s_nalog=ValF(GetGadgetText(#s_nalog))
  ; zp(count(#zp))\s_othisleniya=ValF(GetGadgetText(#s_othisleniya))
   
   oklad.f=zp(count(#zp))\zarplata*12*zp(count(#zp))\vakansii
   
   so.f=(oklad-(zp(count(#zp))\opv/100)*oklad)*(zp(count(#zp))\s_othisleniya/100)
   
   
   sn.f=(oklad-(zp(count(#zp))\opv/100)*oklad)*(zp(count(#zp))\s_nalog/100)-so
   nachislenia.f=oklad+so+sn
   
  sum(#zp)=sum(#zp)+oklad
  


  
 AddGadgetItem(63, count(#zp), Str(count(#zp))+Chr(10)+zp(count(#zp))\dolzhnost+Chr(10)+FormatNumber(zp(count(#zp))\zarplata)+Chr(10)+Str(zp(count(#zp))\vakansii)+Chr(10)+FormatNumber(oklad))
 
 SetGadgetText(#summa,FormatNumber(sum(#zp)))
 
   post(1)\name="зарплата"
   post(1)\price=sum(#zp)
   post(1)\kolvo=1
   post(1)\edizm=""
   ClearGadgetItems(#dolzhnost)
ClearGadgetItems(#zarplata)
  ClearGadgetItems(#vakansii)
EndProcedure

Procedure dobavit_zatraty()
   count(#perem)=count(#perem)+1
   contn=count(#perem)
   
   perem(count(#perem))\name=GetGadgetText(#name)
   perem(count(#perem))\price=ValF(GetGadgetText(#price))
   perem(count(#perem))\kolvo=ValF(GetGadgetText(#kolvo))
   perem(count(#perem))\edizm=GetGadgetText(#edizm)
  
   
   summa.f=perem(count(#perem))\price* perem(count(#perem))\kolvo
   
  sum(#perem)=sum(#perem)+summa
  


  
 AddGadgetItem(73, count(#perem), Str(count(#perem))+Chr(10)+perem(count(#perem))\name+Chr(10)+FormatNumber(perem(count(#perem))\price)+Chr(10)+StrF(perem(count(#perem))\kolvo,3)+Chr(10)+perem(count(#perem))\edizm+Chr(10)+FormatNumber(summa,0))
 
  SetGadgetText(#summazatrat,FormatNumber(sum(#perem)))
  ClearGadgetItems(#name)
  ClearGadgetItems(#price)
  ClearGadgetItems(#kolvo)
  ClearGadgetItems(#edizm)

  
EndProcedure

Procedure dobavit_kap_zatraty()
   count(#kap)=count(#kap)+1
   contn=count(#kap)
   
   kap(count(#kap))\name=GetGadgetText(#gadget_kap_zatrat_name)
   kap(count(#kap))\price=ValF(GetGadgetText(#gadget_kap_zatrat_price))
   kap(count(#kap))\kolvo=ValF(GetGadgetText(#gadget_kap_zatrat_volume))
   kap(count(#kap))\edizm=GetGadgetText(#gadget_kap_zatrat_edizm)
  kap(count(#kap))\srok=Val(GetGadgetText(#spin_srok))
   
   summa.f=kap(count(#kap))\price* kap(count(#kap))\kolvo
   
  sum(#kap)=sum(#kap)+summa
  


  
 AddGadgetItem(#list_kap_zatat, count(3), Str(count(#kap))+Chr(10)+kap(count(#kap))\name+Chr(10)+FormatNumber(kap(count(#kap))\price,0)+Chr(10)+StrF(kap(count(#kap))\kolvo,0)+Chr(10)+kap(count(#kap))\edizm+Chr(10)+FormatNumber(summa,0)+Chr(10)+kap(count(#kap))\srok)
 
  SetGadgetText(#summa_kap_zatrat,FormatNumber(sum(#kap)))
  ClearGadgetItems(#gadget_kap_zatrat_name)
  ClearGadgetItems(#gadget_kap_zatrat_price)
  ClearGadgetItems(#gadget_kap_zatrat_volume)
  ClearGadgetItems(#gadget_kap_zatrat_edizm)
    vlozhenie(1)\name="оборудование"
    vlozhenie(1)\volume=sum(#kap)
     
   iznos(0)
EndProcedure

Procedure dobavit_post_zatraty()
  ClearGadgetItems(#list_post_zatat)
   count(#post)=count(#post)+1
   contn=count(#post)
   sum(#post)=0
   
   post(1)\name="зарплата"
   post(1)\price=sum(#zp)
   post(1)\kolvo=1
   post(1)\edizm=""
   
   post(2)\name="амортизация"
   post(2)\price=sum(#iznos)
   post(2)\kolvo=1
   post(2)\edizm=""
  
    
   post(count(#post))\name=GetGadgetText(#gadget_post_zatrat_name)
   post(count(#post))\price=ValF(GetGadgetText(#gadget_post_zatrat_price))
   post(count(#post))\kolvo=ValF(GetGadgetText(#gadget_post_zatrat_volume))
   post(count(#post))\edizm=GetGadgetText(#gadget_post_zatrat_edizm)
 
   For i=1 To count(#post)
   summa.f=post(i)\price*post(i)\kolvo

  sum(#post)=sum(#post)+summa
 
 AddGadgetItem(#list_post_zatat, i ,Str(i)+Chr(10)+post(i)\name+Chr(10)+FormatNumber(post(i)\price,0)+Chr(10)+StrF(post(i)\kolvo,0)+Chr(10)+post(i)\edizm+Chr(10)+FormatNumber(summa,0))
 Next
  SetGadgetText(#summa_post_zatrat,FormatNumber(sum(#post)))


  
EndProcedure


  
  Procedure add_vlozheniya()
    ClearGadgetItems(#list_vlozheniya)
    sum(#invest)=0
    vlozhenie(1)\name="оборудование"
    vlozhenie(1)\volume=sum(#kap)
    
     
     vlozhenie(count_vlozhenie)\name=GetGadgetText(#gadget_vlozheniya_name)
    vlozhenie(count_vlozhenie)\volume=Val(GetGadgetText(#gadget_vlozheniya_price))
     count_vlozhenie=count_vlozhenie+1
     
    For i=1 To count_vlozhenie-1
   
      AddGadgetItem(#list_vlozheniya,i,vlozhenie(i)\name+Chr(10)+FormatNumber(vlozhenie(i)\volume))
      sum(#invest)=sum(#invest)+vlozhenie(i)\volume
  Next 
  
  raschet_vlozheniya(1)
  
EndProcedure

Procedure raschet_vlozheniya(t)
  If t=1
    ClearGadgetItems(#list_vlozheniya)
  EndIf
  
    sum(#invest)=0
    vlozhenie(1)\name="оборудование"
     vlozhenie(1)\volume=sum(#kap) 
     
     
     For i=1 To count_vlozhenie-1
   
      
      sum(#invest)=sum(#invest)+vlozhenie(i)\volume
    Next 
    
    For i=1 To count_vlozhenie-1
      
      vlozhenie(i)\procent=vlozhenie(i)\volume/sum(#invest)*100
   If t=1
     AddGadgetItem(#list_vlozheniya,i,vlozhenie(i)\name+Chr(10)+FormatNumber(vlozhenie(i)\volume)+Chr(10)+StrF(vlozhenie(i)\procent,2))
     EndIf
   Next 
   
  If t=1
  AddGadgetItem(#list_vlozheniya,i+1,"")
  
  AddGadgetItem(#list_vlozheniya,i+2,"Итого"+Chr(10)+FormatNumber(sum(#invest))+Chr(10)+"100")
  EndIf
  
  
    EndProcedure

Procedure change(n)
  
  k=Val(GetGadgetText(100*n+7))
  sum(n)=0
  If GetGadgetText(100*n+8) <> ""
    zatrat(n,k)\name=GetGadgetText(100*n+8)
  EndIf
  
    If GetGadgetText(100*n+9) <> ""
  zatrat(n,k)\volume=ValF(GetGadgetText(100*n+9))
EndIf

ClearGadgetItems(10*n+3)
  ClearGadgetItems(100*n+7)
  ClearGadgetItems(100*n+8)
   ClearGadgetItems(100*n+9)
 ClearGadgetItems(10*n+4)
   For i=1 To count(n)
     sum(n)=sum(n)+zatrat(n,i)\volume
    AddGadgetItem(10*n+3, i, Str(i)+Chr(10)+zatrat(n,i)\name+Chr(10)+zatrat(n,i)\volume+Chr(10))
  Next i
 SetGadgetText(10*n+4,Str(sum(n)))
EndProcedure

Procedure change_zarplat()
  
  k=Val(GetGadgetText(#redact_nomer_zarplat))
  sum(4)=0
  If GetGadgetText(#redact_nazvanie_zarplat) <> ""
    zp(k)\dolzhnost=GetGadgetText(#redact_nazvanie_zarplat)
  EndIf
  
    If GetGadgetText(#redact_razmer_zarplat) <> ""
  zp(k)\zarplata=ValF(GetGadgetText(#redact_razmer_zarplat))
EndIf

 If GetGadgetText(#redact_kol_vo_vakansii) <> ""
  zp(k)\vakansii=ValF(GetGadgetText(#redact_kol_vo_vakansii))
EndIf

ClearGadgetItems(63)
For i=1 To count(4)
  
   oklad.f=(zp(i)\zarplata-zp(i)\ipn*zp(i)\mzp/100)/((100-zp(i)\opv)*(100-zp(i)\ipn)/10000)
   
   so.f=(oklad-(zp(i)\opv/100)*oklad)*(zp(i)\s_othisleniya/100)
   sn.f=(oklad-(zp(i)\opv/100)*oklad)*(zp(i)\s_othisleniya/100)-so
   nachislenia.f=oklad+so+sn
   

 
  
     sum(4)=sum(4)+zp(i)\vakansii*nachislenia
    AddGadgetItem(63, i, Str(i)+Chr(10)+zp(i)\dolzhnost+Chr(10)+zp(i)\zarplata+Chr(10)+StrF(oklad,2)+Chr(10)+Str(nachislenia)+Chr(10)+Str(zp(i)\vakansii)+Chr(10)+Str(nachislenia*zp(i)\vakansii))
  Next i
 SetGadgetText(#summa,Str(sum(4)))
EndProcedure

Procedure change_zatraty()
  
  k=Val(GetGadgetText(720))
  sum(5)=0
  
  If GetGadgetText(721) <> ""
    zatraty(k)\name=GetGadgetText(721)
  EndIf
  
   If GetGadgetText(722) <> ""
     zatraty(k)\price=ValF(GetGadgetText(722))
  EndIf
  
   If GetGadgetText(723) <> ""
    zatraty(k)\kolvo=Val(GetGadgetText(723))
  EndIf
  
   If GetGadgetText(724) <> ""
    zatraty(k)\edizm=GetGadgetText(724)
  EndIf
  

  
   

ClearGadgetItems(73)
For i=1 To count(5)

  summa.f=zatraty(i)\price* zatraty(i)\kolvo
  
  sum(5)=sum(5)+summa

 AddGadgetItem(73, i, Str(i)+Chr(10)+zatraty(i)\name+Chr(10)+StrF(zatraty(i)\price,0)+Chr(10)+StrF(zatraty(i)\kolvo,0)+Chr(10)+zatraty(i)\edizm+Chr(10)+StrF(summa,0))
Next i

 SetGadgetText(#summazatrat,Str(sum(5)))
 
EndProcedure

Procedure del(n)
  sum(n)=0
  ClearGadgetItems(10*n+3)
  count(n)=count(n)-1
  k=Val(GetGadgetText(100*n+11))
   ClearGadgetItems(100*n+11)
 For i=k To count(n)
   zatrat(n,i)\name=zatrat(n,i+1)\name
   zatrat(n,i)\volume=zatrat(n,i+1)\volume
  Next i

ClearGadgetItems(10*n+4)
  For i=1 To count(n)
    sum(n)=sum(n)+zatrat(n,i)\volume
    AddGadgetItem(10*n+3, i, Str(i)+Chr(10)+zatrat(n,i)\name+Chr(10)+zatrat(n,i)\volume+Chr(10))
  Next i
 SetGadgetText(10*n+4,Str(sum(n)))
EndProcedure

Procedure del_zp()
  sum(4)=0
  
  count(4)=count(4)-1
  
  k=GetGadgetState(63)+1
  
  ClearGadgetItems(63)
 For i=k To count(4)
   zatrat(n,i)\name=zatrat(n,i+1)\name
   zatrat(n,i)\volume=zatrat(n,i+1)\volume
   
   zp(i)\dolzhnost=zp(i+1)\dolzhnost
   zp(i)\zarplata=zp(i+1)\zarplata
   zp(i)\ipn=zp(i+1)\ipn
   zp(i)\opv=zp(i+1)\opv
   zp(i)\mzp=zp(i+1)\mzp
   zp(i)\vakansii=zp(i+1)\vakansii
   
   zp(i)\s_nalog=zp(i+1)\s_nalog
   zp(i)\s_othisleniya=zp(i+1)\s_othisleniya
  Next i


  For i=1 To count(4)
  
   oklad.f=zp(i)\zarplata*12*zp(i)\vakansii
   
   so.f=(oklad-(zp(i)\opv/100)*oklad)*(zp(i)\s_othisleniya/100)
   
   
   sn.f=(oklad-(zp(i)\opv/100)*oklad)*(zp(i)\s_nalog/100)-so
   nachislenia.f=oklad+so+sn
   
  sum(#zp)=sum(#zp)+nachislenia
  


  
 AddGadgetItem(63, i, Str(i)+Chr(10)+zp(i)\dolzhnost+Chr(10)+FormatNumber(zp(i)\zarplata)+Chr(10)+Str(zp(i)\vakansii)+Chr(10)+FormatNumber(oklad)+Chr(10)+FormatNumber(so)+Chr(10)+FormatNumber(sn)+Chr(10)+FormatNumber(nachislenia))
   

 
  
     
  Next i
 SetGadgetText(#summa,Str(sum(4)))
EndProcedure

Procedure del_zatraty()
  
   If count(#perem)<1
    count(#perem)=1
  EndIf
  
  sum(#perem)=0
  k=GetGadgetState(73)+1
  ClearGadgetItems(73)
  count(#perem)=count(#perem)-1
 
    
 For i=k To count(#perem)
   
   
   perem(i)\name=perem(i+1)\name
   perem(i)\price= perem(i+1)\price
   perem(i)\kolvo=perem(i+1)\kolvo
   perem(i)\edizm=perem(i+1)\edizm
   
  Next i


  For i=1 To count(#perem)

  summa.f=perem(i)\price* perem(i)\kolvo
  
  sum(#perem)=sum(#perem)+summa

 AddGadgetItem(73,i, Str(i)+Chr(10)+perem(i)\name+Chr(10)+FormatNumber(perem(i)\price)+Chr(10)+StrF(perem(i)\kolvo,3)+Chr(10)+perem(i)\edizm+Chr(10)+FormatNumber(summa,0))
Next i

 SetGadgetText(#summazatrat,Str(sum(#perem)))
EndProcedure

Procedure del_kap_zatraty()
  
    If count(#kap)<1
    count(#kap)=1
  EndIf
  
  sum(#kap)=0
  
  count(#kap)=count(#kap)-1
  k=GetGadgetState(#list_kap_zatat)+1
  
  ClearGadgetItems(#list_kap_zatat) 
 For i=k To count(#kap)
   
   
   kap(i)\name=kap(i+1)\name
   kap(i)\price= kap(i+1)\price
   kap(i)\kolvo=kap(i+1)\kolvo
   kap(i)\edizm=kap(i+1)\edizm
   
  Next i


  For i=1 To count(#kap)

  summa.f=kap(i)\price* kap(i)\kolvo
  
  sum(#kap)=sum(#kap)+summa

 AddGadgetItem(#list_kap_zatat, i, Str(i)+Chr(10)+kap(i)\name+Chr(10)+FormatNumber(kap(i)\price,0)+Chr(10)+StrF(kap(i)\kolvo,0)+Chr(10)+kap(i)\edizm+Chr(10)+FormatNumber(summa,0)+Chr(10)+kap(i)\srok)
Next i

 SetGadgetText(#summa_kap_zatrat,Str(sum(#kap)))
EndProcedure

Procedure del_post_zatraty()
  
  If count(#post)<1
    count(#post)=1
  EndIf
  
 
  k=GetGadgetState(#list_post_zatat)+1
  If k>2
  ClearGadgetItems(#list_post_zatat)
  count(#post)=count(#post)-1
  sum(#post)=0
   
 For i=k To count(#post)
   
   
   post(i)\name=post(i+1)\name
   post(i)\price= post(i+1)\price
   post(i)\kolvo=post(i+1)\kolvo
   post(i)\edizm=post(i+1)\edizm
   
  Next i


  For i=1 To count(#post)

  summa.f=post(i)\price* post(i)\kolvo
  
  sum(#post)=sum(#post)+summa

 AddGadgetItem(#list_post_zatat, i, Str(i)+Chr(10)+post(i)\name+Chr(10)+FormatNumber(post(i)\price,0)+Chr(10)+StrF(post(i)\kolvo,0)+Chr(10)+post(i)\edizm+Chr(10)+FormatNumber(summa,0))
Next i

SetGadgetText(#summa_post_zatrat,Str(sum(#post)))
EndIf
EndProcedure

Procedure del_vlozn()
   
  k=GetGadgetState(#list_vlozheniya)+1
  If k>1
  ClearGadgetItems(#list_vlozheniya)
  
  count_vlozhenie=count_vlozhenie-1
  
  For i=k To count(#post)
   
   
   vlozhenie(i)\name=vlozhenie(i+1)\name
   vlozhenie(i)\volume=vlozhenie(i+1)\volume
   
   
 Next i
 
  
   For i=1 To count_vlozhenie
   
      ;AddGadgetItem(#list_vlozheniya,i,vlozhenie(i)\name+Chr(10)+FormatNumber(vlozhenie(i)\volume))
      sum(#invest)=sum(#invest)+vlozhenie(i)\volume
    Next
    raschet_vlozheniya(1)
    EndIf
EndProcedure


Procedure tochka(t)
  ;rezhim = GetGadgetState(#rezhim)
 
  
  volume=(sum(#post))/((sum(#perem)*0.2))
   
  ;volume=Val(GetGadgetText(41))
  
  stp=volume/5
  
 vol=0
  p=glob_p
  If t=1
   ClearGadgetItems(46)
  ClearGadgetItems(49)
  EndIf
  
  
  
  For i=0 To 9
    
    vol=vol+stp;пошаговый объем
    ;dat.ocupaemost(i)/vol=vol
    
    post.f=sum(#post)
    ;dat.ocupaemost(i)/post=post
    
    perem.f=vol*(sum(#perem))
    ;dat.ocupaemost(i)/perem=perem
    
    sum.f=post+perem
    ;dat.ocupaemost(i)/sum=sum
    
    
    If rezhim=0
     
       dohod.f=p*vol-p*vol*nalog_dohod
       prib.f=dohod-sum
    ElseIf rezhim=1
      dohod.f=p*vol
      prib.f=(dohod-sum)-(dohod-sum)*nalog_prib
     ElseIf rezhim=2 
    dohod.f=p*vol
    prib.f=dohod-sum
    EndIf
    
    
   
    
    If vol=0
      sebstoi.f=0
      Else
        sebstoi.f=sum(#perem)+sum(#post)/vol
      EndIf
      
      
      
      
  tochka_bezubit=sum(#post)/(p-sum(#perem))
 
    ;dat.ocupaemost(i)/prib=prib
      If t=1
       
    AddGadgetItem(46, i, Str(vol)+Chr(10)+FormatNumber(post)+Chr(10)+FormatNumber(perem)+Chr(10)+FormatNumber(sum)+Chr(10)+FormatNumber(dohod)+Chr(10)+FormatNumber(prib)+Chr(10)+FormatNumber(sebstoi)+Chr(10))
    
    
     If prib>0
       SetGadgetItemColor(46, i,  #PB_Gadget_BackColor,  $33FF33, #PB_All) 
       
    EndIf
    
      If prib<0
        SetGadgetItemColor(46,i,   #PB_Gadget_BackColor,  $6666ff, #PB_All)
        
      EndIf
      
      If prib=0
        SetGadgetItemColor(46,i,   #PB_Gadget_BackColor,  $FF3333, #PB_All)
       
     EndIf
      SetGadgetText(49,FormatNumber( tochka_bezubit))
    EndIf 
     
  
  
  
   If prib>0
      
       toc_hka(i)="<tr ><td class='classgreen'>"+Str(vol)+"</td><td class='classgreen'>"+FormatNumber(post)+"</td><td class='classgreen'>"+FormatNumber(perem)+"</td><td class='classgreen'>"+FormatNumber(sum)+"</td><td class='classgreen'>"+FormatNumber(dohod)+"</td><td  class='classgreen'>"+FormatNumber(prib)+"</td><td class='classgreen'>"+FormatNumber(sebstoi)+"</td></tr>"
    EndIf
    
      If prib<0
        
        toc_hka(i)="<tr ><td class='classred'>"+Str(vol)+"</td><td class='classred'>"+FormatNumber(post)+"</td><td class='classred'>"+FormatNumber(perem)+"</td><td class='classred'>"+FormatNumber(sum)+"</td><td class='classred'>"+FormatNumber(dohod)+"</td><td class='classred'>"+FormatNumber(prib)+"</td><td class='classred'>"+FormatNumber(sebstoi)+"</td></tr>"
      EndIf
      
      If prib=0
        
       toc_hka(i)="<tr ><td class='classred'>"+Str(vol)+"</td><td class='classred'>"+FormatNumber(post)+"</td><td class='classred'>"+FormatNumber(perem)+"</td><td class='classred'>"+FormatNumber(sum)+"</td><td class='classred'>"+FormatNumber(dohod)+"</td><td class='classred'>"+FormatNumber(prib)+"</td><td class='classred'>"+FormatNumber(sebstoi)+"</td></tr>"
     EndIf
      
  Next i
  
    
  optim_point=tochka_bezubit
  EndProcedure   

    
    
  Procedure table_tochka(t)
   
   
    procent=100
    
    p(0)=sum(#perem)
    
    string.s
    
    ;p(0)=p(0)+p(0)/5 
    ;procent=procent+20
  For i=1 To 20
    
  p(i)=p(i-1)+p(0)/10
  procent=procent+10
  

  poin_t(i)=sum(#post)/(p(i)-sum(#perem))


   volume=poin_t(i)*p(i)
   If t=1
     AddGadgetItem(946, i,Str(procent)+"%"+Chr(10)+ FormatNumber(p(i))+Chr(10)+FormatNumber(poin_t(i))+Chr(10)+FormatNumber(volume))
     EndIf
  Next i

  
  optim_price=0
  For i=1 To 10
    
  
  poin_t(i)=sum(#post)/(p(i)-sum(#perem))

   
  
     
  Next


 
  

 
  StopDrawing()
  
  EndProcedure 
  

Procedure.s srok_1(period,price.f,perem.f,post.f,disc.f,kap.f,volume)
  
  npv.f=-kap
  For i=1 To period
    
  
    cf.f=p*volume-perem*volume-post;прибыль
  
  
    dcf.f=cf/(Pow(1+disc,i))       ;дисконтированная прибыль
    npv_hist.f=npv
    npv=npv+dcf
   
    If npv_hist=<0 And npv>0
      
      flag=i-1
      fl.f=dcf
      npvres.f=npv_hist
    EndIf
     
    
  Next i
  
   If flag>4
    res.s=Str(flag)+" лет "+StrF(Abs(npvres/fl)*12,1)+" "+" мес"
  EndIf 
  
  If flag<5 And flag>1
    res.s=Str(flag)+" года "+StrF(Abs(npvres/fl)*12,1)+" "+" мес"
  EndIf 
  
  If flag<=1
    res.s=Str(flag)+" год "+StrF(Abs(npvres/fl)*12,1)+" "+" мес"
  EndIf 
  
  If fl=0
    res.s="нерентабельно"
  EndIf 
  
  ProcedureReturn res
EndProcedure


Procedure.s srok(period,price.f,perem.f,post.f,disc.f,kap.f,volume,rezh)
  
  npv.f=-kap
  
  For i=1 To period
    
    If rezh=0
    cf.f=p*volume-p*volume*nalog_dohod-perem*volume-post;прибыль
  EndIf 
  
 If rezh=1
   cf.f=p*volume-perem*volume-post;прибыль
    cf=cf-cf* nalog_prib
  EndIf 
  
    If rezh=2
    cf.f=p*volume-perem*volume-post;прибыль
  EndIf 
  
    dcf.f=cf/(Pow(1+disc,i))       ;дисконтированная прибыль
     
    npv_hist.f=npv
    npv=npv+dcf
    
    If npv_hist=<0 And npv>0
      
      flag=i-1
      fl.f=dcf
      npvres.f=npv_hist
    EndIf
    
    
  Next i
 
  If flag>4
    res.s=Str(flag)+" лет "+StrF(Abs(npvres/fl)*12,1)+" "+" мес"
  EndIf 
  
  If flag<5 And flag>1
    res.s=Str(flag)+" года "+StrF(Abs(npvres/fl)*12,1)+" "+" мес"
  EndIf 
  
  If flag<=1
    res.s=Str(flag)+" год "+StrF(Abs(npvres/fl)*12,1)+" "+" мес"
  EndIf 
  
  If fl=0
    res.s="нерентабельно"
  EndIf
  
  ProcedureReturn res
EndProcedure

Procedure minimalnyi_ocupaemost()
  volume=(sum(#post))/((sum(#perem)*0.5))
  p=ValF(GetGadgetText(45))
  stp=volume/10
  vol=0
 For i=0 To 9
   vol=vol+stp
    hist_srok.s=srok.s
    
    srok=srok(10,p,sum(#perem),sum(#post),0.1,sum(#invest),vol,rezhim)
    
    
    If srok<>"нерентабельно"
      result=vol
      Break
    EndIf
    
      
Next i
ProcedureReturn result
EndProcedure


            
            
  Procedure NPV(ir.f)
     volume=Val(GetGadgetText(51))
  disc.f=Val(GetGadgetText(53))/100
  p=ValF(GetGadgetText(45))
    For i=1 To 10
      
      cf.f=p*volume-sum(#perem)*volume-sum(#post);прибыль
      
    dcf.f=cf/(Pow(1+ir,i))       ;дисконтированная прибыль
    
    npv=npv+dcf
    
  Next i
  npv=npv-sum(#invest)
  
  ProcedureReturn npv
EndProcedure

 Procedure dNPV(ir.f)
    volume=Val(GetGadgetText(51))
  disc.f=Val(GetGadgetText(53))/100
  p=ValF(GetGadgetText(45))
  
    For i=1 To 10
      
      cf.f=p*volume-sum(#perem)*volume-sum(#post);прибыль
      
    dcf.f=-cf*i/(Pow(1+ir,i+1))       ;дисконтированная прибыль
    
    npv=npv+dcf
    
  Next i
  npv=npv-sum(#invest)
  
  ProcedureReturn npv
EndProcedure

Procedure koeff()
  ;rezhim = GetGadgetState(#rezhim)
  ClearGadgetItems(#pokazatel)
    volume=Val(GetGadgetText(51))
  disc.f=Val(GetGadgetText(53))/100
  p=ValF(GetGadgetText(45))
  
  glob_vol=volume
  glob_desc=disc
  glob_p=p
 irr.f=0.2
  For i=1 To 100
    
    irr=irr-npv(irr)/dNPV(irr)

  Next
  irr=irr*100
  cf.f=p*volume-sum(#perem)*volume-sum(#post)
  
  npv.f=npv(disc)
  
  arr.f=cf/(sum(#invest))*100
  pi.f=npv/(sum(#invest))
  
  

  

  tochka_bezubit=sum(#post)/(p-sum(#perem))


  point.f=tochka_bezubit*p
  
  sebestoi.f=sum(#perem)+sum(#post)/volume
  
  srok.s=srok_1(10,p,sum(#perem),sum(#post),disc,sum(#invest),volume)
  
  
 AddGadgetItem(#pokazatel, 1, "NPV(чистая приведенная стоимость), тенге"+Chr(10)+FormatNumber(npv))
 AddGadgetItem(#pokazatel, 2, "IRR(внутренняя норма доходности),%"+Chr(10)+Str(irr))
 AddGadgetItem(#pokazatel, 3, "PI(индекс рентабельности инвестиций)"+Chr(10)+StrF(pi,2))
 AddGadgetItem(#pokazatel, 4, "ARR(коэффициент эффективности инвестиций),%"+Chr(10)+Str(arr))
 AddGadgetItem(#pokazatel, 5, "PP(срок окупаемости проекта)"+Chr(10)+srok)


 AddGadgetItem(#pokazatel, 6, "Точка окупаемости в натуральном выражении,шт"+Chr(10)+FormatNumber(tochka_bezubit))
 AddGadgetItem(#pokazatel, 7, "Точка окупаемости в денежном выражении,тг"+Chr(10)+FormatNumber(point))
  AddGadgetItem(#pokazatel, 8, "Себестоимость,тг"+Chr(10)+FormatNumber(sebestoi))
 
 koef_doc(0)="<tr><th class='left_stolb'>Показатель</th><th>Значение</th></tr>"
 koef_doc(1)="<tr><td class='left_stolb'>NPV(чистая приведенная стоимость), тенге</td><td>"+FormatNumber(npv)+"</td></tr>"
 koef_doc(2)="<tr><td class='left_stolb'>IRR(внутренняя норма доходности),%</td><td>"+Str(irr)+"</td></tr>"
 koef_doc(3)="<tr><td class='left_stolb'>PI(индекс рентабельности инвестиций)</td><td>"+StrF(pi,2)+"</td></tr>"
 koef_doc(4)="<tr><td class='left_stolb'>ARR(коэффициент эффективности инвестиций),%</td><td>"+Str(arr)+"</td></tr>"
 koef_doc(5)="<tr><td class='left_stolb'>PP(срок окупаемости проекта)</td><td>"+srok+"</td></tr>"
  koef_doc(6)="<tr><td class='left_stolb'>Точка окупаемости в натуральном выражении,шт</td><td>"+FormatNumber(tochka_bezubit)+"</td></tr>"
  koef_doc(7)="<tr><td class='left_stolb'>Точка окупаемости в денежном выражении,тг</td><td>"+FormatNumber(point)+"</td></tr>"
  
EndProcedure


Procedure ocupaimost(t)
  If t=1
  ClearGadgetItems(56)
  ClearGadgetItems(59)
EndIf

  volume=glob_vol
  disc.f=glob_desc
  p=glob_p
  ;rezhim = GetGadgetState(#rezhim)
 
  
  st=10
  
   
   
   
     For i=1 To st
       discont(i)=disc
       Next
 
     
     
     For i=1 To st
       vol(i)=volume
       Next
  
     
     
     
     For i=1 To st
       
      
         cf=p*vol(i)-(sum(#perem))*vol(i)-(sum(#post));расчет прибыли
        
    
    dcf.f=cf/(Pow(1+discont(i),i))
    npv.f=npv+dcf
    If t=1
    AddGadgetItem(56, i-1, Str(i)+Chr(10)+FormatNumber(sum(#invest))+Chr(10)+FormatNumber(cf)+Chr(10)+FormatNumber(dcf)+Chr(10)+FormatNumber(npv)+Chr(10)+FormatNumber(npv-sum(#invest))+Chr(10))
    EndIf 
    If (npv-sum(#invest))>=0
       If t=1
         SetGadgetItemColor(56,  i-1, #PB_Gadget_BackColor,  $33FF33, #PB_All) 
         EndIf
      ocu_paimost(i-1)="<tr><td class='classgreen'>"+Str(i)+"</td><td class='classgreen'>"+FormatNumber(sum(#invest))+"</td><td class='classgreen'>"+FormatNumber(cf)+"</td><td class='classgreen'>"+FormatNumber(dcf)+"</td><td class='classgreen'>"+FormatNumber(npv)+"</td><td class='classgreen'>"+FormatNumber(npv-sum(#invest))+"</td></tr>"
    EndIf
    
    If (npv-sum(#invest))<0
       If t=1
         SetGadgetItemColor(56,  i-1, #PB_Gadget_BackColor,  $6666ff, #PB_All)
         EndIf
        ocu_paimost(i-1)="<tr><td class='classred'>"+Str(i)+"</td><td class='classred'>"+FormatNumber(sum(#invest))+"</td><td class='classred'>"+FormatNumber(cf)+"</td><td class='classred'>"+FormatNumber(dcf)+"</td><td class='classred'>"+FormatNumber(npv)+"</td><td class='classred'>"+FormatNumber(npv-sum(#invest))+"</td></tr>"
      EndIf
      
      ;If (npv-sum(#invest))=0
       ; SetGadgetItemColor(56,  i-1, #PB_Gadget_BackColor,  $FF3333, #PB_All)
        ;ocu_paimost(i-1)="<tr><td>"+Str(i)+"</td><td>"+Str(sum(#invest))+"</td><td>"+Str(cf)+"</td><td>"+Str(dcf)+"</td><td>"+Str(npv)+"</td><td class='classred'>"+Str(npv-sum(#invest))+"</td></tr>"
      ;EndIf
      
  Next i
  ocup=srok_1(st,p,sum(#perem),sum(#post),disc,sum(#invest),volume)
  If t=1
  SetGadgetText(59,ocup)
  EndIf

  
EndProcedure  

Procedure table(t)
  If t=1
    ClearGadgetItems(66)
    EndIf
  ;ClearGadgetItems(69)
  Dim tocka_.s(20)  
  volume=(sum(#post))/((sum(#perem)*0.2))
   
  ;volume=Val(GetGadgetText(41))
  
  stp=volume/10
  
 vol=0
  p=glob_p
  ;rezhim = GetGadgetState(#rezhim)
  
 
  st=10
  
  For i=0 To 19
    vol=vol+stp
    
   
    
    tocka_(1)=srok(st,p,sum(#perem),sum(#post),0.025,sum(#invest),vol,rezhim)
    tocka_(2)=srok(st,p,sum(#perem),sum(#post),0.05,sum(#invest),vol,rezhim)
    tocka_(3)=srok(st,p,sum(#perem),sum(#post),0.075,sum(#invest),vol,rezhim)
    tocka_(4)=srok(st,p,sum(#perem),sum(#post),0.1,sum(#invest),vol,rezhim)
    tocka_(5)=srok(st,p,sum(#perem),sum(#post),0.125,sum(#invest),vol,rezhim)
    tocka_(6)=srok(st,p,sum(#perem),sum(#post),0.15,sum(#invest),vol,rezhim)
    
    If tocka_(6)<>"нерентабельно"
       If t=1
    AddGadgetItem(66, i, Str(vol)+Chr(10)+tocka_(1)+Chr(10)+tocka_(2)+Chr(10)+tocka_(3)+Chr(10)+tocka_(4)+Chr(10)+tocka_(5)+Chr(10)+tocka_(6)+Chr(10))
EndIf
    table_ocupaimost(i)="<tr ><td >"+Str(vol)+"</td ><td >"+tocka_(1)+"</td ><td >"+tocka_(2)+"</td ><td >"+tocka_(3)+"</td ><td >"+tocka_(4)+"</td ><td >"+tocka_(5)+"</td ><td >"+tocka_(6)+"</td ></tr >"
    EndIf
     
  Next i
  
 
  vol=0
  
  
  ;SetGadgetText(69,Str(minimalnyi_ocupaemost()))
   ;SetGadgetText(51,Str(minimalnyi_ocupaemost()))
EndProcedure 


Procedure save_doc()
  
   
        dir.s= SaveFileRequester("Сохранить отчет", "save.doc", "doc", Pattern)
      
  
  
        If CreateFile(0, dir,#PB_Ascii)         ; we create a new text file...
          
          WriteStringN(0,"<html>" )
          WriteStringN(0,"<head>" )
          WriteStringN(0,"<style type='text/css'>" )
          
          WriteStringN(0,"table{" )
          WriteStringN(0,"font-family: 'Times New Roman';" )
          WriteStringN(0,"font-size: 65%;" )
          WriteStringN(0,"padding-top: 10px;" )
          WriteStringN(0,"padding-right: 10px;" )
          WriteStringN(0,"padding-bottom: 10px;" )
          WriteStringN(0,"padding-left: 10px;" )
          WriteStringN(0,"text-align: center;" )
          WriteStringN(0,"border-collapse: collapse;}" )
          
          WriteStringN(0,"th{" )
          WriteStringN(0,"background: #AFCDE7;}" )       
          WriteStringN(0,"td{" )
          WriteStringN(0,"background: #D8E6F3;}" )
          
            WriteStringN(0,"th, td {" )
          WriteStringN(0,"border-style: solid;" )       
          WriteStringN(0,"border-width: 0 2px 2px 0;" )
          WriteStringN(0,"border-color: black;}" )
          
           WriteStringN(0,".left_stolb{" )
          WriteStringN(0,"background: #AFCDE7;" )       
          WriteStringN(0,"text-align: left;}" )
          WriteStringN(0,".classred{background: #ff6666;}")
          WriteStringN(0,".classgreen{background: #33FF33;}")
          WriteStringN(0,".classblue{background: #FF3333;}")
          WriteStringN(0,"</style></head>" )
          
          
          WriteStringN(0,"<body>" )
          WriteStringN(0,"<h5>Капитальные затраты</h5>" )
          WriteStringN(0,"<table width='100%'>" )
          
           WriteStringN(0,"<tr>" )        
           WriteStringN(0,"<th class='left_stolb'>Наименование</th>" )
           WriteStringN(0,"<th>Цена, тенге</th>" )
           WriteStringN(0,"<th>Количество, шт</th>" )
           WriteStringN(0,"<th>Стоимость, тенге</th>" )
           WriteStringN(0,"</tr>" )
           
           For i=1 To count(#kap)
           WriteStringN(0,"<tr>" )        
           WriteStringN(0,"<td class='left_stolb'>"+kap(i)\name+"</td>" )
           WriteStringN(0,"<td>"+FormatNumber(kap(i)\price)+"</td>" )
           WriteStringN(0,"<td>"+Str(kap(i)\kolvo)+"</td>" )
           summa.f=kap(i)\price* kap(i)\kolvo
           WriteStringN(0,"<td>"+FormatNumber(summa)+"</td>" )
           WriteStringN(0,"</tr>" )
           Next
           WriteStringN(0,"</table>" )
           
           ;-------------------------------------------------------------------------------------износ 
           WriteStringN(0,"<h5>Износ оборудования</h5>" )
          WriteStringN(0,"<table width='100%'>" )
           WriteStringN(0,"<tr>" )        
           WriteStringN(0,"<th class='left_stolb'>Вид оборудования</th>" )
           WriteStringN(0,"<th>Начальная стоимость</th>" )
           WriteStringN(0,"<th>Срок эксплуатации, лет</th>" )
           WriteStringN(0,"<th>Ликвидационная стоимость, тенге</th>" )
           WriteStringN(0,"<th>Годовая сумма амортизационных отчислений,  тенгее</th>" )
           WriteStringN(0,"</tr>" )
          
           For i=1 To count(#kap)
    
    likvid=kap(i)\price*0.1*kap(i)\kolvo
    godovaya_amartizazzia=(kap(i)\price*0.9*kap(i)\kolvo)/kap(i)\srok
    
    sum_likvid=sum_likvid+likvid
    sum_ga=sum_ga+godovaya_amartizazzia
    ;AddGadgetItem(#list_iznos, i,kap(i)\name+Chr(10)+StrF(kap(i)\price*kap(i)\kolvo)+Chr(10)+StrF(kap(i)\srok)+Chr(10)+Str(likvid)+Chr(10)+StrF(godovaya_amartizazzia,0))
     WriteStringN(0,"<tr>" )        
           WriteStringN(0,"<td class='left_stolb'>"+kap(i)\name+"</td>" )
           WriteStringN(0,"<td>"+FormatNumber(kap(i)\price*kap(i)\kolvo)+"</td>" )
           WriteStringN(0,"<td>"+StrF(kap(i)\srok)+"</td>" )
           
           WriteStringN(0,"<td>"+FormatNumber(likvid)+"</td>" )
           WriteStringN(0,"<td>"+FormatNumber(godovaya_amartizazzia,0)+"</td>" )
           
           WriteStringN(0,"</tr>" )
  Next
   WriteStringN(0,"<tr>" )        
           WriteStringN(0,"<th class='left_stolb'>Итого</th>" )
           WriteStringN(0,"<th>"+FormatNumber(sum(#kap))+"</th>" )
           WriteStringN(0,"<th></th>" )         
           WriteStringN(0,"<th>"+FormatNumber(sum_likvid)+"</th>" )
           WriteStringN(0,"<th>"+FormatNumber(sum_ga)+"</th>" )           
           WriteStringN(0,"</tr>" )           
           WriteStringN(0,"</table>" )
           
           ;-------------------------------------------------------------------------------------вложения 
           
           WriteStringN(0,"<h5>Структура вложений</h5>" )
          WriteStringN(0,"<table width='100%'>" )
          
           WriteStringN(0,"<tr>" )        
           WriteStringN(0,"<th class='left_stolb'>Объект инвестиций</th>" )
           WriteStringN(0,"<th>Стоимость, тенге</th>" )
           WriteStringN(0,"<th>Структура, %</th>" )           
           WriteStringN(0,"</tr>" )
           
           For i=1 To count_vlozhenie-1
           WriteStringN(0,"<tr>" )        
           WriteStringN(0,"<td class='left_stolb'>"+vlozhenie(i)\name+"</td>" )
           WriteStringN(0,"<td>"+FormatNumber(vlozhenie(i)\volume)+"</td>" )
           WriteStringN(0,"<td>"+Str(vlozhenie(i)\procent)+"</td>" )           
           WriteStringN(0,"</tr>" )
         Next
         
         WriteStringN(0,"<tr>" )        
           WriteStringN(0,"<th class='left_stolb'>Итого</th>" )
           WriteStringN(0,"<th>"+FormatNumber(sum(#invest))+"</th>" )
           WriteStringN(0,"<th>100</th>" )           
           WriteStringN(0,"</tr>" )
           
           WriteStringN(0,"</table>" )
           ;-------------------------------------------------------------------------------------зарплата
            
       WriteStringN(0,"<h5>Зарплата</h5>" )
          WriteStringN(0,"<table width='100%'>" )
          
           WriteStringN(0,"<tr>" )        
           WriteStringN(0,"<th class='left_stolb'>Должность</th>" )
           WriteStringN(0,"<th>Зарплата, тенге</th>" )
           WriteStringN(0,"<th>Кол-во работников</th>" )
           WriteStringN(0,"<th>Годовой фонд</th>" )
           WriteStringN(0,"<th>Соц.отчисления</th>" )
           WriteStringN(0,"<th>Соц.налог</th>" )
           WriteStringN(0,"<th>Годовой фонд с отчислениями</th>" )
           WriteStringN(0,"</tr>" )
           
           For i=1 To count(#zp)
           WriteStringN(0,"<tr>" )        
           WriteStringN(0,"<td class='left_stolb'>"+zp(i)\dolzhnost+"</td>" )
           WriteStringN(0,"<td>"+FormatNumber(zp(i)\zarplata)+"</td>" )
           WriteStringN(0,"<td>"+Str(zp(i)\vakansii)+"</td>" )  
           
           
           oklad.f=zp(i)\zarplata*12*zp(i)\vakansii
   
   so.f=(oklad-(zp(i)\opv/100)*oklad)*(zp(i)\s_othisleniya/100)
   
   
   sn.f=(oklad-(zp(i)\opv/100)*oklad)*(zp(i)\s_nalog/100)-so
   nachislenia.f=oklad+so+sn
   
   WriteStringN(0,"<td>"+FormatNumber(oklad)+"</td>" )
   WriteStringN(0,"<td>"+FormatNumber(so)+"</td>" )
   WriteStringN(0,"<td>"+FormatNumber(sn)+"</td>" )
   WriteStringN(0,"<td>"+FormatNumber(nachislenia)+"</td>" )
   WriteStringN(0,"</tr>" )
         Next
         
         WriteStringN(0,"<tr>" )        
           WriteStringN(0,"<th class='left_stolb'>Итого</th>" )
           WriteStringN(0,"<th></th><th></th><th></th><th></th><th></th>" )          
           WriteStringN(0,"<th>"+FormatNumber(sum(#zp))+"</th>" ) 
           WriteStringN(0,"</table>" )
      
;-----------------------------------------------------------------------переменные издержки   
   WriteStringN(0,"<h5>Затраты на единицу продукции</h5>" )
          WriteStringN(0,"<table width='100%'>" )
          
           WriteStringN(0,"<tr>" )        
           WriteStringN(0,"<th class='left_stolb'>Наименование</th>" )
           WriteStringN(0,"<th>Цена, тенге</th>" )
           WriteStringN(0,"<th>Кол-во </th>" )
           WriteStringN(0,"<th>Единица измерения</th>" )
           WriteStringN(0,"<th>Стоимость</th>" )
           
           WriteStringN(0,"</tr>" )
           
           
           For i=1 To count(#perem)
             WriteStringN(0,"<tr>" ) 
              WriteStringN(0,"<td class='left_stolb'>"+perem(i)\name+"</td>" )
              WriteStringN(0,"<td>"+FormatNumber(perem(i)\price)+"</td>" )
              WriteStringN(0,"<td>"+StrF(perem(i)\kolvo,2)+"</td>" )
              WriteStringN(0,"<td>"+perem(i)\edizm+"</td>" )
              summa.f=perem(i)\price*perem(i)\kolvo
              
              WriteStringN(0,"<td>"+FormatNumber(summa,0)+"</td>" )
              WriteStringN(0,"</tr>" ) 
           Next
           
           WriteStringN(0,"<tr>" )        
           WriteStringN(0,"<th class='left_stolb'>Итого</th>" )
           WriteStringN(0,"<th></th><th></th><th></th>" )  
           WriteStringN(0,"<th>"+FormatNumber(sum(#perem))+"</th>" ) 
  
   WriteStringN(0,"</table>" )
 
;-----------------------------------------------------------------------постоянные издержки              
   WriteStringN(0,"<h5>Постоянные затраты</h5>" )
   
     WriteStringN(0,"<table width='100%'>" )
          
           WriteStringN(0,"<tr>" )        
           WriteStringN(0,"<th class='left_stolb'>Наименование</th>" )
           WriteStringN(0,"<th>Цена, тенге</th>" )
           WriteStringN(0,"<th>Кол-во </th>" )
           WriteStringN(0,"<th>Единица измерения</th>" )
           WriteStringN(0,"<th>Стоимость</th>" )
           
           WriteStringN(0,"</tr>" )
   
   
           For i=1 To count(#post)
             
             WriteStringN(0,"<tr>" )        
           WriteStringN(0,"<td class='left_stolb'>"+post(i)\name+"</th>" )
           WriteStringN(0,"<td>"+FormatNumber(post(i)\price,0)+"</td>" )
           WriteStringN(0,"<td>"+StrF(post(i)\kolvo,0)+"</td>" )
           WriteStringN(0,"<td>"+post(i)\edizm+"</td>" )
           
          summa.f=post(i)\price*post(i)\kolvo
           WriteStringN(0,"<td>"+FormatNumber(summa,0)+"</td>" )
           WriteStringN(0,"</tr>" )
           Next
           
           WriteStringN(0,"<tr>" )        
           WriteStringN(0,"<th class='left_stolb'>Итого</th><th ></th><th></th><th ></th>" )
          
           
          
           WriteStringN(0,"<th>"+FormatNumber(sum(#post))+"</th>" )
           WriteStringN(0,"</tr>" )
   
   WriteStringN(0,"</table>" ) 
   
   
 ;----------------------------------------------кривая безубыточности 
   WriteStringN(0,"<h5>Кривая  безубыточности, расчет точки безубыточности при различной стоимости товара</h5>" )
   
     WriteStringN(0,"<table width='100%'>" )
          
           WriteStringN(0,"<tr>" )        
           WriteStringN(0,"<th class='left_stolb'>Наценка%</th>" )
           WriteStringN(0,"<th>Цена, тенге</th>" )
           WriteStringN(0,"<th>Точка окупаемости</th>" )
           WriteStringN(0,"<th>Объем продаж</th>" )
     
           WriteStringN(0,"</tr>" )
           
           
           procent=100
           For i=1 To 20
             procent=procent+10
             WriteStringN(0,"<tr>" )        
           WriteStringN(0,"<td class='left_stolb'>"+Str(procent)+"</th>" )
           WriteStringN(0,"<td>"+FormatNumber(p(i))+"</td>" )
           WriteStringN(0,"<td>"+FormatNumber(poin_t(i),2)+"</td>" )
           
           
           volume=poin_t(i)*p(i)
           
           WriteStringN(0,"<td>"+FormatNumber(volume)+"</td>" )
           WriteStringN(0,"</tr>" )
         Next
         
         WriteStringN(0,"</table>" )
         
 ;----------------------------------------------точка безубыточности 
   WriteStringN(0,"<h5>Точка  безубыточности </h5>" )
   
     WriteStringN(0,"<table width='100%'>" )
          
           WriteStringN(0,"<tr>" )        
           WriteStringN(0,"<th class='left_stolb'>Объем</th>" )
           WriteStringN(0,"<th>Постоянные издержки</th>" )
           WriteStringN(0,"<th>Переменные издержки</th>" )
           WriteStringN(0,"<th>Общие издержки</th>" )
            WriteStringN(0,"<th>Доход</th>" )
           WriteStringN(0,"<th>Прибыль</th>" )
           WriteStringN(0,"<th>Себестоимость</th>" )
           WriteStringN(0,"</tr>" )
            
           
           For i=0 To 19
              WriteStringN(0,toc_hka(i) )
            Next 
           WriteStringN(0,"</table>" ) 
        WriteStringN(0,"<h6>Точка безубыточности при данной цене  "+ FormatNumber(tochka_bezubit)+" штук</h6>" )
     
          
 ;----------------------------------------------анализ окупаемости
          WriteStringN(0,"<h5>Анализ окупаемости при различных объемах продаж</h5>" )
   
     WriteStringN(0,"<table width='100%'>" )
          
           WriteStringN(0,"<tr>" )        
           WriteStringN(0,"<th class='left_stolb'>Объем/Ставка дисконтирования</th>" )
           WriteStringN(0,"<th>2.5%</th>" )
           WriteStringN(0,"<th>5%</th>" )
           WriteStringN(0,"<th>7.5%</th>" )
            WriteStringN(0,"<th>10%</th>" )
           WriteStringN(0,"<th>12.5%</th>" )
           WriteStringN(0,"<th>15%</th>" )
           WriteStringN(0,"</tr>" )       
         
         
          For i=0 To 19
              WriteStringN(0,table_ocupaimost(i) )
            Next 
            WriteStringN(0,"</table>" ) 
             
             ;----------------------------------------------окупаемость
             
           WriteStringN(0,"<h5>Таблица окупаемости по годам</h5>" )
   
     WriteStringN(0,"<table width='100%'>" )
          
           WriteStringN(0,"<tr>" )        
           WriteStringN(0,"<th class='left_stolb'>Период</th>" )
           WriteStringN(0,"<th>Капитальные затраты</th>" )
           WriteStringN(0,"<th>Денежный поток</th>" )
           WriteStringN(0,"<th>Дисконтированный денежный поток</th>" )
           WriteStringN(0,"<th>Денежный поток нарастающим итогом</th>" )
            WriteStringN(0,"<th>Окупаемость</th>" )
          
            For i=0 To 9
              WriteStringN(0,ocu_paimost(i) )
            Next 
            WriteStringN(0,"</table>" ) 
            
            WriteStringN(0,"<h6>Cрок окупаемости проекта  "+ocup +"</h6>" )
            
            ;----------------------------------------------коэффициенты
             WriteStringN(0,"<h5>Основные показатели</h5>" )
             WriteStringN(0,"<table width='100%'>" )
             For i=0 To 10
                WriteStringN(0,koef_doc(i))
             Next
             WriteStringN(0,"</table>" )
         
         
         
         
         
         
          WriteStringN(0,"</body></html>" )
          
   CloseFile(0)

    
   
 Else
   MessageRequester("Ошибка","Невозможно создать файл")
   EndIf
  

  directory=dir
  
EndProcedure


Procedure save(flag)
  
  If flag=1 
        dir.s= SaveFileRequester("Please choose file to save", "save.snp", "snp", Pattern)
      
  Else
    dir=directory
  EndIf
  
  If CreateFile(0, dir)         ; we create a new text file...
    
    For i=1 To  count(#kap)
      WriteStringN(0,"kap" )
      WriteStringN(0,kap(i)\name )
      WriteStringN(0,StrF(kap(i)\price) )
      WriteStringN(0,StrF(kap(i)\kolvo) )
      WriteStringN(0,kap(i)\edizm )
      WriteStringN(0,StrF(kap(i)\srok ))
    Next
    
    For i=1 To  count(#perem)
      WriteStringN(0,"perem" )
      WriteStringN(0,perem(i)\name )
      WriteStringN(0,StrF(perem(i)\price) )
      WriteStringN(0,StrF(perem(i)\kolvo) )
      WriteStringN(0,perem(i)\edizm )
     
    Next
   
    For i=1 To  count(#post)
      WriteStringN(0,"post" )
      WriteStringN(0,post(i)\name )
      WriteStringN(0,StrF(post(i)\price) )
      WriteStringN(0,StrF(post(i)\kolvo) )
      WriteStringN(0,post(i)\edizm )
     
    Next
    
      For i=1 To  count(#zp)
      WriteStringN(0,"zp" )
      WriteStringN(0,zp(i)\dolzhnost)
      WriteStringN(0,Str(zp(i)\zarplata))
      WriteStringN(0,Str(zp(i)\vakansii))
      WriteStringN(0,StrF(zp(i)\opv))
      WriteStringN(0,StrF(zp(i)\s_nalog))
      WriteStringN(0,StrF(zp(i)\s_othisleniya))
     
    Next
    
    For i=1 To  count_vlozhenie-1
      WriteStringN(0,"vlozh" )
      WriteStringN(0,vlozhenie(i)\name)
 WriteStringN(0,Str(vlozhenie(i)\volume))
     
Next

 CloseFile(0)                       ; close the previously opened file and store the written data this way
    
  Else
    MessageRequester("Information","may not create the file!")
  EndIf
  directory=dir
  
EndProcedure

Procedure load()
  For n=1 To 9
   
   
   count(n)=0
   sum(n)=0
 Next
 ;count(1)=2

 count_vlozhenie=1
   ;ClearGadgetItems(63)
   ;ClearGadgetItems(73)
 
 ;ClearGadgetItems(#summazatrat)
 ;ClearGadgetItems(#list_post_zatat)
 
 dir.s= OpenFileRequester("Please choose file to load", "save.snp", "snp", Pattern)
 directory=dir
  Dim text.s(500)
   
i=0
  
  If ReadFile(0, dir)   ; if the file could be read, we continue...
    While Eof(0) = 0           ; loop as long the 'end of file' isn't reached
      text(i) =ReadString(0)   ; display line by line in the debug window
      
      i=i+1
    Wend
     
    For j=0 To i ;Step 3
     
      If text(j)="kap"
     count(#kap)=count(#kap)+1 
     
    
    kap(count(#kap))\name=text(j+1)
   kap(count(#kap))\price=ValF(text(j+2)) 
   kap(count(#kap))\kolvo=ValF(text(j+3))
   kap(count(#kap))\edizm=text(j+4)
   kap(count(#kap))\srok=Val(text(j+5)) 
   
   summa.f=kap(count(#kap))\price*kap(count(#kap))\kolvo  
            sum(#kap)=sum(#kap)+summa   
  
  
  
 
    
    
  ElseIf text(j)="perem"
    
    count(#perem)=count(#perem)+1
   contn=count(#perem)
   
   perem(count(#perem))\name=text(j+1)
   perem(count(#perem))\price=ValF(text(j+2)) 
   perem(count(#perem))\kolvo=ValF(text(j+3))
   perem(count(#perem))\edizm=text(j+4)
   summa.f=perem(count(#perem))\price* perem(count(#perem))\kolvo   
  sum(#perem)=sum(#perem)+summa
  
  
  
  
  
  
  
  
  
ElseIf text(j)="post"
  
   
   
   
  count(#post)=count(#post)+1
  contn=count(#post)
  
  post(count(#post))\name=text(j+1)
   post(count(#post))\price=ValF(text(j+2)) 
   post(count(#post))\kolvo=ValF(text(j+3))
   post(count(#post))\edizm=text(j+4)
  
  
   summa.f=post(count(#post))\price*post(count(#post))\kolvo

  sum(#post)=sum(#post)+summa
 

    
    ElseIf text(j)="zp"
   count(#zp)=count(#zp)+1
   contn=count(#zp)
   
   zp(count(#zp))\dolzhnost=text(j+1)
   zp(count(#zp))\zarplata=ValF(text(j+2))
   ;zp(count(4))\ipn=ValF(GetGadgetText(#ipn))
   zp(count(#zp))\vakansii=ValF(text(j+3)) 
   zp(count(#zp))\opv=ValF(text(j+4))
   ;zp(count(4))\mzp=ValF(GetGadgetText(#mzp))
    zp(count(#zp))\s_nalog=ValF(text(j+5))
    zp(count(#zp))\s_othisleniya=ValF(text(j+6))
    oklad.f=zp(count(#zp))\zarplata*12*zp(count(#zp))\vakansii
     so.f=(oklad-(zp(count(#zp))\opv/100)*oklad)*(zp(count(#zp))\s_othisleniya/100)
   
   
   sn.f=(oklad-(zp(count(#zp))\opv/100)*oklad)*(zp(count(#zp))\s_nalog/100)-so
   nachislenia.f=oklad+so+sn
   
     sum(#zp)=sum(#zp)+oklad
 
    
     
    ElseIf text(j)="vlozh"
   vlozhenie(count_vlozhenie)\name=text(j+1)
    vlozhenie(count_vlozhenie)\volume=ValF(text(j+2))
     sum(#invest)=sum(#invest)+vlozhenie(count_vlozhenie)\volume
     
   
   
      
 
  count_vlozhenie=count_vlozhenie+1 
    
    
  EndIf
    Next j
    
    
   ; iznos()
    CloseFile(0)               ; close the previously opened file
  Else
    MessageRequester("Information","Couldn't open the file!")
  EndIf


EndProcedure

Procedure panel_kap()
  ;AddGadgetItem(0, #window_kap, "Капитальные затраты")
  OpenWindow(#window_kap, 0, 0, 1200, 600, "Капитальные затраты",#PB_Window_SystemMenu|#PB_Window_WindowCentered, WindowID(#main_window) ) 
  ; FrameGadget(88, 10,  10, 1000, 500, "Капитальные затраты (IC)")
 ;ContainerGadget(89, 10, 30, 1000, 600, #PB_Container_Raised)
           y=20
          TextGadget(#text_kap_zatrat_name, 10, 00+y,400, 15,"наименование" )
          EditorGadget(#gadget_kap_zatrat_name, 10, 15+y, 400, 20 )
          SetGadgetText(#gadget_kap_zatrat_name,"здание")
          
          TextGadget(#text_kap_zatrat_price, 415, 00+y, 100, 15,"цена" )
          EditorGadget(#gadget_kap_zatrat_price, 415, 15+y, 100, 20 )
          SetGadgetText(#gadget_kap_zatrat_price,"1000000")
          
          TextGadget(#text_kap_zatrat_volume, 520, 00+y, 100, 20,"кол-во" )
          SpinGadget(#gadget_kap_zatrat_volume, 520, 15+y, 100, 20,1,100 ,#PB_Spin_Numeric)
          SetGadgetText(#gadget_kap_zatrat_volume,"1")
          SetGadgetState(#gadget_kap_zatrat_volume,1)
          
          TextGadget(#text_kap_zatrat_edizm, 625, 00+y, 100, 15,"ед изм" )
          EditorGadget(#gadget_kap_zatrat_edizm, 625, 15+y, 100, 20 )
          SetGadgetText(#gadget_kap_zatrat_edizm,"шт")
          
          TextGadget(#text_srok, 730, 00+y, 100, 20,"срок эксплуатации" )
          
          SpinGadget(#spin_srok, 730,15+y,100,20, 1, 20,#PB_Spin_Numeric)
          SetGadgetText(#spin_srok,"10")
          SetGadgetState(#spin_srok,10)
         
          ButtonGadget(#add_kap_zatrat, 850, 15+y, 80, 20,"добавить")
          
          ListIconGadget(#list_kap_zatat, 10,  50+y, 900, 300, "# ", 30, #PB_ListIcon_GridLines|#PB_ListIcon_FullRowSelect)
          AddGadgetColumn(#list_kap_zatat, 1, "наименование ", 340)
          AddGadgetColumn(#list_kap_zatat, 2, "цена ", 100)
          AddGadgetColumn(#list_kap_zatat, 3, "количество ", 100)
          AddGadgetColumn(#list_kap_zatat, 4, "единица ", 100)
          AddGadgetColumn(#list_kap_zatat, 5, "стоимость ", 100)
          AddGadgetColumn(#list_kap_zatat, 6, "срок эксплуатации ", 100)
          
          EditorGadget(#summa_kap_zatrat, 160, 360+y, 100, 20 )
          TextGadget(#text_summa_kap_zatrat, 10, 360+y, 100, 20,"Сумма " )
          
          
          
          
       

          ButtonGadget(#but_del_kap, 10, 420+y, 60, 20,"удалить")
          
          ;CloseGadgetList()
            For i=1 To count(#kap)
            summa.f=kap(i)\price*kap(i)\kolvo  
            ;sum(#kap)=sum(#kap)+summa   
            AddGadgetItem(#list_kap_zatat, i, Str(i)+Chr(10)+kap(i)\name+Chr(10)+FormatNumber(kap(i)\price,0)+Chr(10)+StrF(kap(i)\kolvo,0)+Chr(10)+kap(i)\edizm+Chr(10)+FormatNumber(summa,0)+Chr(10)+kap(i)\srok)
 
            SetGadgetText(#summa_kap_zatrat,FormatNumber(sum(#kap)))
          Next
         
          
          EndProcedure
          
 Procedure panel_vlozh()
   OpenWindow(#window_vlozh, 0, 0, 1200, 600, "Вложения",#PB_Window_SystemMenu|#PB_Window_WindowCentered, WindowID(#main_window) )   
   ;AddGadgetItem(0, #window_vlozh, "Вложения")
   y=20       
        
          
 ;FrameGadget(#vlozheniya_frame, 10,  10, 1200, 500, "Вложения")
 ;ContainerGadget(#vlozheniya_container, 10, 30, 1200, 600, #PB_Container_Raised)
 
 TextGadget(#text_vlozheniya_name, 10, 00+y, 400, 15,"наименование" )
          EditorGadget(#gadget_vlozheniya_name, 10, 15+y, 400, 20 )
          SetGadgetText(#gadget_vlozheniya_name,"оборотные средства")
          
          TextGadget(#text_vlozheniya_price, 415, 00+y, 100, 15,"цена" )
          EditorGadget(#gadget_vlozheniya_price, 415, 15+y, 100, 20 )
          SetGadgetText(#gadget_vlozheniya_price,"1000000")
          
          
         
          ;ButtonGadget(#button_vlozheniya, 850, 15, 80, 20,"рассчет")
          ButtonGadget(#button_add_vlozheniya, 650, 15+y, 80, 20,"добавить")
          
          ListIconGadget(#list_vlozheniya, 10,  50+y, 800, 300, "Объект инвестиций ", 150, #PB_ListIcon_GridLines|#PB_ListIcon_FullRowSelect)
          AddGadgetColumn(#list_vlozheniya, 1, "стоимость ", 150)
          AddGadgetColumn(#list_vlozheniya, 2, "структура ", 150)
          
          vlozhenie(1)\name="оборудование"
        
   
          
          ;EditorGadget(#summa_kap_zatrat, 160, 360, 100, 20 )
          ;TextGadget(#text_summa_kap_zatrat, 10, 360, 100, 20,"Сумма " )
           ButtonGadget(#button_del_vlozh, 10, 420+y, 60, 20,"удалить")
          ;CloseGadgetList() 
          
          For i=1 To count_vlozhenie-1
            vlozhenie(i)\procent=vlozhenie(i)\volume/sum(#invest)*100
         AddGadgetItem(#list_vlozheniya,i,vlozhenie(i)\name+Chr(10)+FormatNumber(vlozhenie(i)\volume)+Chr(10)+FormatNumber(vlozhenie(i)\procent))
      
       Next
       
        AddGadgetItem(#list_vlozheniya,i+1,"")
  
        AddGadgetItem(#list_vlozheniya,i+2,"Итого"+Chr(10)+FormatNumber(sum(#invest))+Chr(10)+"100")
        
        raschet_vlozheniya(1)
        
EndProcedure
          
Procedure panel_zp()
  y=20
  OpenWindow(#window_zp, 0, 0, 1200, 600, "Зарплата" ,#PB_Window_SystemMenu|#PB_Window_WindowCentered, WindowID(#main_window) ) 
  ;AddGadgetItem(0, #window_zp, "Зарплата")
  ;FrameGadget(680, 10,  10, 1000, 500, "Зарплата, суммируется к постоянным издержкам")
 ;ContainerGadget(690, 10, 30, 1000, 600, #PB_Container_Raised)

          TextGadget(601, 10, 00+y, 400, 15,"должность" )
          EditorGadget(#dolzhnost, 10, 15+y, 400, 20 )
          SetGadgetText(#dolzhnost,"инженер")
          
          TextGadget(603, 415, 00+y, 100, 15,"зарплата" )
          EditorGadget(#zarplata, 415, 15+y, 100, 20 )
          SetGadgetText(#zarplata,"200000")
          
          TextGadget(605, 520, 00+y, 100, 15,"кол-во вакансий" )
          ;EditorGadget(#vakansii, 220, 15, 100, 20 )
          ;SetGadgetText(#vakansii,"1")
          
          SpinGadget(#vakansii, 520,15+y,100,20, 1, 50,#PB_Spin_Numeric)
         SetGadgetText(#vakansii,"1")
         SetGadgetState(#vakansii,1)
         
       
          
          
          ButtonGadget(#add_zp, 850, 15+y, 80, 20,"добавить")
          
          ListIconGadget(63, 10,  50+y, 1000, 300, "# ", 30, #PB_ListIcon_GridLines|#PB_ListIcon_FullRowSelect)
          AddGadgetColumn(63, 1, "должность ", 400)
          AddGadgetColumn(63, 2, "зарплата ", 130)
          AddGadgetColumn(63, 3, "кол-во работников ", 130)
          AddGadgetColumn(63, 4, "годовой фонд ", 130)
          ;AddGadgetColumn(63, 5, "соц.отчисления ", 130)
          ;AddGadgetColumn(63, 6, "соц. налог ", 130)
          ;AddGadgetColumn(63, 7, "годовой фонд с отчислениями ", 130)
          
          EditorGadget(#summa, 160, 360+y, 100, 20 )
          TextGadget(65, 10, 360+y, 100, 20,"Сумма " )
          
          ;611 612
          
          ;EditorGadget(#del_kap_number, 00, 500, 20, 20)
          
          TextGadget(#text_kap_zatrat_volume, 10, 400+y, 100, 20,"Удалить строку" )
          
          
          ButtonGadget(612, 10, 420+y, 60, 20,"удалить")
          
          ;CloseGadgetList() 
          
          
          
       For i=1 To count(#zp)  
   oklad.f=zp(i)\zarplata*12*zp(i)\vakansii  
   
   
   so.f=(oklad-(zp(i)\opv/100)*oklad)*(zp(i)\s_othisleniya/100)
   
   
   sn.f=(oklad-(zp(i)\opv/100)*oklad)*(zp(i)\s_nalog/100)-so
   
   nachislenia.f=oklad+so+sn

 AddGadgetItem(63, i, Str(i)+Chr(10)+zp(i)\dolzhnost+Chr(10)+FormatNumber(zp(i)\zarplata)+Chr(10)+Str(zp(i)\vakansii)+Chr(10)+FormatNumber(oklad)+Chr(10)+FormatNumber(so)+Chr(10)+FormatNumber(sn)+Chr(10)+FormatNumber(nachislenia))
Next 

  SetGadgetText(#summa,FormatNumber(sum(#zp)))
  EndProcedure
  
  Procedure panel_perem()
    y=20
    OpenWindow(#window_perem, 0, 0, 1200, 600, "Перменные затраты",#PB_Window_SystemMenu|#PB_Window_WindowCentered, WindowID(#main_window) )
    ;AddGadgetItem(0, #window_perem, "Перменные затраты")
   

          TextGadget(701, 10, 00+y, 400, 15,"наименование" )
          EditorGadget(#name, 10, 15+y, 400, 20 )
          SetGadgetText(#name,"сырье")
          
          TextGadget(703, 415, 00+y, 100, 15,"цена" )
          EditorGadget(#price, 415, 15+y, 100, 20 )
          SetGadgetText(#price,"5000")
          
          TextGadget(705, 520, 00+y, 100, 15,"кол-во" )
          ;EditorGadget(#kolvo, 520, 15, 100, 20 )
         
          
          SpinGadget(#kolvo, 520, 15+y, 100, 20,1,100,#PB_Spin_Numeric   )
          SetGadgetText(#kolvo,"10")
          SetGadgetState(#kolvo,10)
          
          TextGadget(726, 625, 00+y, 100, 15,"ед изм" )
          EditorGadget(#edizm, 625, 15+y, 100, 20 )
          SetGadgetText(#edizm,"кг")
          
          
          
          ButtonGadget(#add_zatrat, 850, 15+y, 80, 20,"добавить")
          
          ListIconGadget(73, 10,  50+y, 900, 300, "# ", 30, #PB_ListIcon_GridLines|#PB_ListIcon_FullRowSelect)
          AddGadgetColumn(73, 1, "наименование ", 300)
          AddGadgetColumn(73, 2, "цена ", 130)
          AddGadgetColumn(73, 3, "количество ", 130)
          AddGadgetColumn(73, 4, "единица ", 130)
          AddGadgetColumn(73, 5, "стоимость ", 130)
          
          
          EditorGadget(#summazatrat, 160, 360+y, 100, 20 )
          TextGadget(75, 10, 360+y, 100, 20,"Сумма " )
          
        
          
          ButtonGadget(712, 10, 420+y, 60, 20,"удалить")
         
          
          
          
          For i=1 To count(#perem)
          summa.f=perem(i)\price* perem(i)\kolvo   
  AddGadgetItem(73, i, Str(i)+Chr(10)+perem(i)\name+Chr(10)+FormatNumber(perem(i)\price)+Chr(10)+StrF(perem(i)\kolvo,3)+Chr(10)+perem(i)\edizm+Chr(10)+FormatNumber(summa,0))
  Next 
  
  SetGadgetText(#summazatrat,FormatNumber(sum(#perem)))
  
  EndProcedure
  
  Procedure panel_post()
    OpenWindow(#window_post, 0, 0, 1200, 600, "Постоянные затраты",#PB_Window_SystemMenu|#PB_Window_WindowCentered, WindowID(#main_window)  ) 
    y=20
    ;AddGadgetItem(0, #window_post, "Постоянные затраты")
   ; FrameGadget(#frame_post_zatrat, 10,  10, 1000, 500, "Постоянные затраты (IC)")
; ContainerGadget(#container_post_zatrat, 10, 30+y, 1000, 600, #PB_Container_Raised)

          TextGadget(#text_post_zatrat_name, 10, 00+y, 400, 15,"наименование" )
          EditorGadget(#gadget_post_zatrat_name, 10, 15+y, 400, 20 )
          SetGadgetText(#gadget_post_zatrat_name,"реклама")
          
          TextGadget(#text_post_zatrat_price, 415, 00+y, 100, 15,"цена" )
          EditorGadget(#gadget_post_zatrat_price, 415, 15+y, 100, 20 )
          SetGadgetText(#gadget_post_zatrat_price,"100000")
          
          TextGadget(#text_post_zatrat_volume, 520, 00+y, 100, 15,"кол-во" )
          SpinGadget(#gadget_post_zatrat_volume, 520, 15+y, 100, 20,1,100,#PB_Spin_Numeric )
          SetGadgetText(#gadget_post_zatrat_volume,"1")
          SetGadgetState(#gadget_post_zatrat_volume,1)
          
          TextGadget(#text_post_zatrat_edizm, 625, 00+y, 100, 15,"ед изм" )
          EditorGadget(#gadget_post_zatrat_edizm, 625, 15+y, 100, 20 )
          SetGadgetText(#gadget_post_zatrat_edizm,"шт")
          
          
         
          ButtonGadget(#add_post_zatrat, 850, 15+y, 80, 20,"добавить")
          
          ListIconGadget(#list_post_zatat, 10,  50+y, 900, 300, "# ", 30, #PB_ListIcon_GridLines|#PB_ListIcon_FullRowSelect)
          AddGadgetColumn(#list_post_zatat, 1, "наименование ", 300)
          AddGadgetColumn(#list_post_zatat, 2, "цена ", 130)
          AddGadgetColumn(#list_post_zatat, 3, "количество ", 130)
          AddGadgetColumn(#list_post_zatat, 4, "единица ", 130)
          AddGadgetColumn(#list_post_zatat, 5, "стоимость ", 130)
          ;AddGadgetColumn(#list_post_zatat, 6, "срок эксплуатации ", 130)
          
          EditorGadget(#summa_post_zatrat, 160, 360+y, 100, 20 )
          TextGadget(#text_summa_post_zatrat, 10, 360+y, 100, 20,"Сумма " )
          
           TextGadget(#text_kap_zatrat_volume, 10, 400+y, 100, 20,"Удалить строку" )
        
          ButtonGadget(#but_del_post, 10, 420+y, 60, 20,"удалить")
          
          
          
          
          ;CloseGadgetList() 
          
          
          
          For i=1 To count(#post)
          summa.f=post(i)\price*post(i)\kolvo    
 AddGadgetItem(#list_post_zatat, i ,Str(i)+Chr(10)+post(i)\name+Chr(10)+FormatNumber(post(i)\price,0)+Chr(10)+StrF(post(i)\kolvo,0)+Chr(10)+post(i)\edizm+Chr(10)+FormatNumber(summa,0))
 Next
 SetGadgetText(#summa_post_zatrat,FormatNumber(sum(#post)))
 
 
  EndProcedure
  
  Procedure panel_iznos()
    
    ;AddGadgetItem(0, #window_iznos, "Перменные затраты") 
     OpenWindow(#window_iznos, 0, 0, 900, 400, "Перменные затраты" ,#PB_Window_SystemMenu|#PB_Window_WindowCentered, WindowID(#main_window) ) 
    ;FrameGadget(#iznos_frame, 10,  10, 1000, 500, "Износ оборудования")
 ;ContainerGadget(#iznos_container, 10, 30, 1000, 600, #PB_Container_Raised)

          
         
          ;ButtonGadget(#button_add_iznos, 850, 15, 80, 20,"рассчет")
          
          ListIconGadget(#list_iznos, 10,  50, 800, 300, "Вид оборудования ", 150, #PB_ListIcon_GridLines)
          AddGadgetColumn(#list_iznos, 1, "Начальная стоимость ", 150)
          AddGadgetColumn(#list_iznos, 2, "Срок эксплуатации ", 150)
          AddGadgetColumn(#list_iznos, 3, "Ликвидационная стоимость", 150)
          AddGadgetColumn(#list_iznos, 4, "Годовая амортизации ", 150)
         
          
          ;EditorGadget(#summa_kap_zatrat, 160, 360, 100, 20 )
          ;TextGadget(#text_summa_kap_zatrat, 10, 360, 100, 20,"Сумма " )
          
         
          
          ;CloseGadgetList() 
          iznos(1)
    EndProcedure
    
    Procedure menu()
  
  
  If CreateMenu(0, WindowID(#main_window))
    MenuTitle("Файл")
      
      MenuItem( 2, "Открыть")
      MenuItem( 3, "Сохранить")
      MenuItem( 4, "Сохранить как..")
    
      
      MenuItem( 5, "Сохранить отчет в Word")
      
       MenuTitle("Добавить")
      MenuItem( #menu_kap, "Капитальные затраты")
      MenuItem( #menu_vlozh, "Прочие вложения")
      MenuItem( #menu_zp, "Зарплата")
      MenuItem( #menu_perem, "Переменные затраты")
      MenuItem( #menu_post, "Постоянные затраты")
      
      
      
      MenuTitle("Таблицы")
      MenuItem(#menu_koef,"Основные показатели")
      MenuItem( #menu_iznos, "Амортизация")
      MenuItem( #menu_table_tochka, "Таблица безубыточности")
      MenuItem( #menu_tochka, "Точка безубыточности")
      MenuItem( #menu_table_ocup, "Таблица окупаемости")
      MenuItem( #menu_ocup, "Cрок окупаемости")
    EndIf
    
  EndProcedure
  
  Procedure action()
    
      If directory=""
        DisableMenuItem(0, 3, 1)
      EndIf
      
      If directory<>""
        DisableMenuItem(0, 3, 0)
      EndIf
    
     Select WaitWindowEvent()

      Case #PB_Event_Menu

          Select EventMenu()  ; To see which menu has been selected
              Case 2
                load()
              Case 3 ; сохранить
                save(0)
                
              Case 4 ; сохранить как
                save(1)
              Case 5
                save_doc()
              Case #menu_kap
                panel_kap()
              Case #menu_vlozh
                panel_vlozh()
              Case #menu_zp
                panel_zp()
              Case #menu_perem
                panel_perem()
              Case #menu_post
                panel_post()
              Case #menu_iznos
                panel_iznos()
              Case #menu_table_tochka
                panel_table_tochka()
                Case #menu_tochka
                  panel_tochka()
                Case #menu_table_ocup
                  panel_table_ocup()
                  Case #menu_ocup
                  panel_ocup()
                Case #menu_koef
                  panel()
          EndSelect
        
       Case #PB_Event_Gadget
          
          Select EventGadget()  ; To see which menu has been selected
           
              
            Case #button_del_vlozh
                
                  del_vlozn()
              
            Case #but_del_kap
              del_kap_zatraty()
              Case #but_del_post
              del_post_zatraty()
              
              
                
             
                
              Case 2000 ; About
                load()
                CloseWindow(5)
              Case #button_add_iznos
                iznos(1)
              Case #add_kap_zatrat
                dobavit_kap_zatraty()
              Case #button_vlozheniya
                raschet_vlozheniya(1)
              Case #add_zp ; About
                dobavit_zp()
                
              Case #add_post_zatrat
                dobavit_post_zatraty()
                
              Case #button_add_vlozheniya
                add_vlozheniya()
                
                Case #del_zp_button
                 del_zp()
                  
                Case #redact_zp ; About
                change_zarplat()
                
                Case #add_zatrat ; About
                  dobavit_zatraty()
                  
                  Case 710 ; About
                    change_zatraty()
                    
                  Case 712
                    del_zatraty()
                    
             
           
            
             
                    
              Case #raschet
                koeff()
                
                  check_open_winow()
                  
          EndSelect
          
      Case #PB_Event_CloseWindow
        Select EventWindow()  ; To see which menu has been selected

          Case #main_window
            CloseWindow(#main_window)
        End
        
         
             
           Case(#window_kap)
             CloseWindow(#window_kap)
             
           Case(#window_vlozh)
             CloseWindow(#window_vlozh)
               
           Case(#window_zp)
             CloseWindow(#window_zp)
                 
           Case(#window_perem)
             CloseWindow(#window_perem)
                 
           Case(#window_post)
             CloseWindow(#window_post)
             
            Case(#window_iznos)
              CloseWindow(#window_iznos)
              
              Case(#window_table_tochka)
                CloseWindow(#window_table_tochka)
                
                 Case(#window_tochka)
                   CloseWindow(#window_tochka)
                   
                    Case(#window_table_ocup)
                      CloseWindow(#window_table_ocup)
                    Case(#window_ocup)
                      CloseWindow(#window_ocup)
                      Case(#window_koef)
                      CloseWindow(#window_koef)
        EndSelect

    EndSelect

    EndProcedure
    
    Procedure panel()
      
OpenWindow(#window_koef, 0, 0, 500, 600, "Основные показатели" ,#PB_Window_SystemMenu|#PB_Window_WindowCentered, WindowID(#main_window) )
         ;AddGadgetItem(0, #window_koef, "Основные показатели")
          
          TextGadget(44, 10, 30, 200, 20,"Стоимость единицы товара (P)" )
          EditorGadget(45, 250, 30, 100, 20 )
          SetGadgetText(45,"60000")
          
          TextGadget(50, 10, 70, 240, 20,"Планируемый объем продаж (в среднем)" )
          EditorGadget(51, 250, 70, 100, 20 )
         SetGadgetText(51,"800")
    
          
          TextGadget(52, 10, 110, 200, 20,"Ставка дисконтирования (в среднем)" )
          EditorGadget(53, 250, 110, 100, 20 )
          SetGadgetText(53,"10")
       
          ButtonGadget(#raschet, 10, 500, 80, 20,"рассчитать")

           ListIconGadget(#pokazatel, 10,  150, 420, 300, "Показатели", 250, #PB_ListIcon_GridLines)
           AddGadgetColumn(#pokazatel, 1, "Значение", 160)
           
  koeff()
  EndProcedure
  
  Procedure panel_table_tochka()
    
     OpenWindow(#window_table_tochka, 0, 0, 600, 500, "Таблица безубыточности" ,#PB_Window_SystemMenu|#PB_Window_WindowCentered, WindowID(#main_window) ) 
    ;AddGadgetItem(0, #window_table_tochka, "Таблица безубыточности" )
      
          
         
          
          ListIconGadget(946, 100,  10, 400, 450, "наценка %", 100, #PB_ListIcon_GridLines)
          
        
           AddGadgetColumn(946, 1, "цена ", 100)
             AddGadgetColumn(946, 2, "точка ", 100)
             AddGadgetColumn(946, 3, "объем ", 100)
          
           ;TextGadget(948, 10, 700, 150, 20,"Оптимальная цена" )
           ;EditorGadget(949, 150, 700, 100, 20 )
             table_tochka(1)
           
    EndProcedure
    
    Procedure panel_table_ocup()
      OpenWindow(#window_table_ocup, 0, 0, 800, 400, "Таблица окупаемости" ,#PB_Window_SystemMenu|#PB_Window_WindowCentered, WindowID(#main_window) ) 
         ;AddGadgetItem(0, #window_table_ocup, "Таблица окупаемости" ) 
          ListIconGadget(66, 10,  10, 700, 300, "Объем", 60, #PB_ListIcon_GridLines)
          AddGadgetColumn(66, 1, "2.5%", 100)
          AddGadgetColumn(66, 2, "5%", 100)
          AddGadgetColumn(66, 3, "7.5%", 100)
          AddGadgetColumn(66, 4, "10%", 100)
          AddGadgetColumn(66, 5, "12.5%", 100)
          AddGadgetColumn(66, 6, "15%", 100)
          
          table(1)
    EndProcedure
    
Procedure panel_tochka()
      ;AddGadgetItem(0, #window_tochka, "Точка безубыточности"  ) 
       OpenWindow(#window_tochka, 0, 0, 1200, 400, "Точка безубыточности",#PB_Window_SystemMenu|#PB_Window_WindowCentered, WindowID(#main_window)  ) 
      ListIconGadget(46, 10,  10, 1200, 300, "объем ", 160, #PB_ListIcon_GridLines)
          AddGadgetColumn(46, 1, "Постоянные издержки (FC)", 160)
          AddGadgetColumn(46, 2, "Переменные издержки (VC)", 160)
          AddGadgetColumn(46, 3, "Общие издержки (TC)", 160)
          AddGadgetColumn(46, 4, "Доход (TR)", 160)
          AddGadgetColumn(46, 5, "Прибыль (CF)", 160)
          AddGadgetColumn(46, 6, "Себестоимость", 160)
          
           TextGadget(48, 10, 350, 150, 20,"Точка безубыточности" )
          EditorGadget(49, 160, 350, 100, 20 )

           tochka(1)
          
      EndProcedure
      
Procedure panel_ocup()
  OpenWindow(#window_ocup, 0, 0, 1100, 400, "Срок окупаемости",#PB_Window_SystemMenu|#PB_Window_WindowCentered, WindowID(#main_window)  )   
  ;AddGadgetItem(0, #window_ocup, "Таблица окупаемости" ) 
   ListIconGadget(56, 10,10, 1000, 300, "Период ", 160, #PB_ListIcon_GridLines)
          AddGadgetColumn(56, 1, "Капитальные затраты (IC)", 160)
          AddGadgetColumn(56, 2, "Денежный поток (CF)", 160)
          AddGadgetColumn(56, 3, "Дисконтированный поток (DCF)", 160)
          AddGadgetColumn(56, 4, "Поток нарастающим итогом (NPV)", 160)
          AddGadgetColumn(56, 5, "Окупаемость", 160)
          
           TextGadget(58, 10, 360, 150, 20,"Срок окупаемости" )
           EditorGadget(59, 160, 360, 100, 20 )
           ocupaimost(1)
EndProcedure
      
Procedure check_open_winow()
  If (IsWindow(#window_tochka))
                    tochka(1)
                  Else
                    tochka(0)
                  EndIf
                  
     If (IsWindow(#window_table_ocup))
                    table(1)
                  Else
                    table(0)
                  EndIf
                  
       If (IsWindow(#window_ocup))
                    ocupaimost(1)
                  Else
                   ocupaimost(0)
                  EndIf            
                
                  ocupaimost(0)
                  table_tochka(0)
                  iznos(0)
                  raschet_vlozheniya(0)
 EndProcedure
 
 ExamineDesktops()
    

        
              
If OpenWindow(#main_window, 0, 0, 0, 0, "SinApp Investo" ,#PB_Window_SystemMenu |#PB_Window_MinimizeGadget | #PB_Window_MaximizeGadget|#PB_Window_Maximize)
SetWindowColor(#main_window, RGB(150, 150, 150))
 


  menu()
 
  
  
  Repeat

  action()
  
  
  
ForEver
EndIf
End  

; IDE Options = PureBasic 5.60 (Windows - x64)
; CursorPosition = 992
; FirstLine = 223
; Folding = AAAAAAA+
; EnableXP
; UseIcon = ..\icon.ico
; Executable = ..\sinapp_invest1111.exe