Structure port
  name_connection.s
  number_connection.i
  ip.s
  tcp.i
EndStructure 

Structure tag
  name_tag.s
  number_divice.s
  function.s
  start_reg_hi.s
  start_reg_lo.s
  leght_hi.s
  leght_lo.s
  type.i
  number_connection.i
  byte.i
EndStructure 

Structure shema
  positionX.i
  positionY.i
  type.i
EndStructure

Structure pis
  number.i
  text.s
  color.i
  fon.i
EndStructure

Structure nye
  number.i
  tag.i
  bit.i
  EndStructure
 

    
    Structure parametr_graf
      number.i
      tag.i
      EndStructure
  
Global ConnectionID, r, Dim connect.port(10),count_create_port=1, Dim modbus.tag(100),count_create_tag=1, Dim mnemo.shema(100),type_element,thread
Global count_element_mnemoshem=1,Dim nad.pis(100),count_nadpis=0, Dim dannie.nye(100),count_okno_dannih,Dim type_dannih.s(3), color, fon, Dim oprosm(100)
Global Dim discret.nye(100), Dim discret_oprosm(8,100), Dim number_analog_tag(100), Dim number_discret_tag(100), count_discret_input, number_element,thread_pusk
Global out_thread, Dim grafik.parametr_graf(100), count_grafik, Dim time.s(100)
InitNetwork()


Declare graf(x,y,z)

Procedure create_connection(Port_tcp,ip.s,n)
  

If OpenNetworkConnection(ip, Port_tcp)
  connect(n)\number_connection = OpenNetworkConnection(ip, Port_tcp)
  MessageRequester("Message","Соединение создано")
  flag=1
Else
  flag=0
MessageRequester("Error","Порт не найден")
EndIf
ProcedureReturn flag
EndProcedure

type_dannih(1)="BOOL"
  type_dannih(2)="INT"
  type_dannih(3)="REAL"
  
Enumeration 
  #add_node=2000
  #window_parametr_node
  #text_name_node
  #edit_name_node
  
  
  
  #button_parametr_node
  #button_create_nadpis
  #window_create_nadpis
  #text_quantity
  #spin_quantity
  
  #edit_tags
  #window_create_port
  #add_port
  #add_tag
  #text_IP_tag
  #edit_IP_tag
  #text_tcp_port
  #gadget_tcp_port
  #create_port
  #table_tag
  #text_name_connection
  #edit_name_connection
  #table_connection
  #window_create_tag
  #text_name_tag
  #edit_name_tag
  #create_tag
  #text_connection_tag
  #edit_connection_tag
  #text_number_device_tag
  #edit_number_device_tag
   #text_function_tag
   #edit_function_tag
   #text_registr_tag
   #edit_registr_tag_hi
   #edit_registr_tag_lo
   #text_lenght_tag
   #edit_lenght_tag_hi
   #edit_lenght_tag_lo
   #menu_connection
   #menu_tag
   #edit_type_tag
   #text_type_tag
   #position_mouse
   #panel_connect
   #menu_indow_connection
   #panel_tag
   #menu_indow_tag
   #canvas
   #combo_element
   
   #window_create_okno_dannih
   #text_create_okno_dannih
   #spin_create_okno_dannih
   #button_create_okno_dannih
   #menu_okno_dannyh
   #pusk
   #stop
   #window_pusk
   #canvas_pusk
   #nadpis_color
   #nadpis_fon
   
   #window_create_discret
   #text_create_discret
   #spin_create_discret
   #button_create_discret
   #menu_okno_discret
   #spin_byte_number
   #text_create_byte
   #window_create_graf
   #text_create_graf
   #spin_create_graf
   #button_create_graf
   #menu_okno_graf
EndEnumeration  
  
Global m
;--------------------------------------узлы

 Procedure modbus_tcp(a.s,b.s,c.s,d.s,e.s,f.s,n)  
;*Buffer = AllocateMemory(1000)
*b = AllocateMemory(1000)
Dim dat.s(10)

dat(0)=a
dat(1)=b
dat(2)=c
dat(3)=d
dat(4)=e
dat(5)=f


If connect(n)\number_connection
  
  
 

  
  
  Start$="0"

     PokeB(*b,$00) 
     PokeB(*b+1,$00) 
     
     PokeB(*b+2,$00) 
     PokeB(*b+3,$00) 
     
     PokeB(*b+4,$00)
     PokeB(*b+5,$06);
     
     PokeB(*b+6,Val("$" + dat(0)));адрес устройства
     
     PokeB(*b+7,Val("$" + dat(1))) ;функция
     
     PokeB(*b+8,Val("$" + dat(2))) ; start high
     PokeB(*b+9,Val("$" + dat(3))) ; start low
     
     PokeB(*b+10,Val("$" + dat(4))) ;  
     PokeB(*b+11,Val("$" + dat(5))) ;  
     
      
  
  

    flag=SendNetworkData(connect(n)\number_connection,*b,12)
  
  
  
    
 
  
    
  Delay(100)
  
  If NetworkClientEvent(connect(n)\number_connection) =#PB_NetworkEvent_Data
   
    
    i=ReceiveNetworkData(connect(n)\number_connection, *b, 1000)
    ClearDebugOutput()
    
       ; For j=1 To i  
          ;Debug(Right("0" + Hex(PeekB(*Buffer+j)),2)) 
          ; Next 
    
  
  EndIf
  
  result=PeekB(*b+10)
  
  ;CloseNetworkConnection(ConnectionID)
  

  
Else
  
EndIf
FreeMemory(*b) 



    ProcedureReturn result
  
 EndProcedure 
  
    
Procedure parametr_element(number_element)
  Select number_element
      Case 1
      OpenWindow(#window_create_nadpis, 100, 100, 600, 400, "параметры элемента" ,#PB_Window_SystemMenu | #PB_Window_MinimizeGadget | #PB_Window_MaximizeGadget)    
      TextGadget(#text_quantity,10,10,80,30, "надпись")
      EditorGadget(#spin_quantity,200,10,80,20)      
      type_element=1
      ButtonGadget(#nadpis_color, 10, 40, 100, 30, "цвет шрифта")
      ButtonGadget(#nadpis_fon, 10, 80, 100, 30, "цвет фона")
      
      ButtonGadget(#button_create_nadpis, 10, 300, 100, 30, "создать")
      
    Case 2
      OpenWindow(#window_create_okno_dannih, 100, 100, 600, 400, "параметры элемента" ,#PB_Window_SystemMenu | #PB_Window_MinimizeGadget | #PB_Window_MaximizeGadget) 
      TextGadget(#text_create_okno_dannih,10,10,80,30, "тег")
      ComboBoxGadget( #spin_create_okno_dannih,210,10,120,20) 
      For i = 1 To count_create_tag-1  
         
           AddGadgetItem( #spin_create_okno_dannih, -1,modbus(i)\name_tag+"."+type_dannih(modbus(i)\type))
          
      Next
     
      type_element=2
      ButtonGadget(#button_create_okno_dannih, 10, 300, 100, 30, "создать")
      
      
    Case 3
      OpenWindow(#window_create_discret, 100, 100, 600, 400, "параметры элемента" ,#PB_Window_SystemMenu | #PB_Window_MinimizeGadget | #PB_Window_MaximizeGadget) 
      TextGadget(#text_create_discret,10,10,80,30, "тег")
      ComboBoxGadget( #spin_create_discret,210,10,120,20) 
      For i = 1 To count_create_tag-1 
        
          AddGadgetItem( #spin_create_discret, -1,modbus(i)\name_tag+"."+type_dannih(modbus(i)\type))
          
        Next
        TextGadget(#text_create_byte,10,40,80,30, "бит")
     SpinGadget(#spin_byte_number, 210,40,120,20, 0, 7)
      type_element=3
      ButtonGadget(#button_create_discret, 10, 300, 100, 30, "создать")
    Case 4
       OpenWindow(#window_create_graf, 100, 100, 600, 400, "параметры элемента" ,#PB_Window_SystemMenu | #PB_Window_MinimizeGadget | #PB_Window_MaximizeGadget) 
      TextGadget(#text_create_graf,10,10,80,30, "тег")
      ComboBoxGadget( #spin_create_graf,210,10,120,20) 
      For i = 1 To count_create_tag-1  
         
           AddGadgetItem( #spin_create_graf, -1,modbus(i)\name_tag+"."+type_dannih(modbus(i)\type))
          
      Next
     
      type_element=4
      ButtonGadget(#button_create_graf, 10, 300, 100, 30, "создать")
  EndSelect
  EndProcedure
  
Procedure font(number_element,font)
    Select number_element
      Case 1
        
        Select font
          Case 1
            color_hist=color
            
            color = ColorRequester(color)
            
              If color<0
                color=color_hist
              EndIf
              
            Case 0
              fon_hist=fon
               fon = ColorRequester(fon)
              If fon<0
                fon=fon_hist
              EndIf 
       EndSelect
   EndSelect 
   
    
EndProcedure
  
Procedure save_parametr(number_element)
   Select number_element
   Case 1
    count_nadpis=count_nadpis+1
    nad(count_nadpis)\number=count_nadpis
    nad(count_nadpis)\text=GetGadgetText(#spin_quantity)
    nad(count_nadpis)\color=color
    nad(count_nadpis)\fon=fon
   
  Case 2
    count_okno_dannih=count_okno_dannih+1
    dannie(count_okno_dannih)\number=count_okno_dannih
    dannie(count_okno_dannih)\tag=GetGadgetState(#spin_create_okno_dannih)+1
    
  Case 3
    count_discret_input=count_discret_input+1
   discret(count_discret_input)\number=count_discret_input
   discret(count_discret_input)\tag=GetGadgetState(#spin_create_discret)+1
   discret(count_discret_input)\bit=GetGadgetState(#spin_byte_number)
   
   Case 4
     count_grafik=count_grafik+1
     grafik(count_grafik)\number=count_grafik
     grafik(count_grafik)\tag=GetGadgetState(#spin_create_graf)+1
  EndSelect
EndProcedure
  
  
Procedure draw_mnemoshem()
  For i=10 To 1200 Step 10
    For j=10 To 800 Step 10
      Circle(i, j, 1,RGB(0,0,0))
    Next
  Next
  
  
      For i = 1 To count_element_mnemoshem
        Select  mnemo(i)\type
          Case 1
            count_nadpis_mnemoshem=count_nadpis_mnemoshem+1
            
            red=Red(nad.pis(count_nadpis_mnemoshem)\color)
            green=Green(nad.pis(count_nadpis_mnemoshem)\color)
            blue=Blue(nad.pis(count_nadpis_mnemoshem)\color)
            red_fon=Red(nad.pis(count_nadpis_mnemoshem)\fon)
            green_fon=Green(nad.pis(count_nadpis_mnemoshem)\fon)
            blue_fon=Blue(nad.pis(count_nadpis_mnemoshem)\fon)
            
              
              DrawText(mnemo(i)\positionX,mnemo(i)\positionY,nad.pis(count_nadpis_mnemoshem)\text,RGB(red, green, blue),RGB(red_fon, green_fon, blue_fon))
           Case 2
              count_okno_dannih_mnemoshem=count_okno_dannih_mnemoshem+1
               Box(mnemo(i)\positionX, mnemo(i)\positionY, 80, 30, RGB(0, 0, 0))
               Box(mnemo(i)\positionX+2, mnemo(i)\positionY+2, 80-4, 30-4, RGB(255, 255, 255))
             Case 3
               count_discret_input_mnemoshem=count_discret_input_mnemoshem+1
             Circle(mnemo(i)\positionX, mnemo(i)\positionY, 15,RGB(0,0,0))
             Circle(mnemo(i)\positionX, mnemo(i)\positionY, 12,RGB(255,0,0))
           Case 4
             count_graf_mnemoshem=count_graf_mnemoshem+1
             Box(mnemo(i)\positionX,mnemo(i)\positionY, 400, 200, RGB(0, 0, 0))
             Box(mnemo(i)\positionX+2,mnemo(i)\positionY+2, 400-4, 200-4, RGB(255, 255, 255))
          EndSelect
        Next
      
        count_nadpis_mnemoshem=0
        count_okno_dannih_mnemoshem=0
        count_discret_input_mnemoshem=0
EndProcedure
    
Procedure vibor_pozicii(number_element)
  
   time_pause=100
    Select number_element
        Case 1
      Repeat
       StartDrawing(CanvasOutput(#canvas))
       out_thread=1
       Box(0, 0, 1200, 800, RGB(255, 255, 255))

       ;count_nadpis_mnemoshem=count_nadpis_mnemoshem+1
            
            red=Red(nad.pis(count_nadpis)\color)
            green=Green(nad.pis(count_nadpis)\color)
            blue=Blue(nad.pis(count_nadpis)\color)
            red_fon=Red(nad.pis(count_nadpis)\fon)
            green_fon=Green(nad.pis(count_nadpis)\fon)
            blue_fon=Blue(nad.pis(count_nadpis)\fon)
            
              
             DrawText(WindowMouseX(0), WindowMouseY(0), nad.pis(count_nadpis)\text,RGB(red, green, blue),RGB(red_fon, green_fon, blue_fon))

             draw_mnemoshem()
            
       StopDrawing()
       out_thread=0
        Delay(time_pause)
     ForEver
     
   Case 2
     Repeat
       StartDrawing(CanvasOutput(#canvas))
       out_thread=1
       
      
         Box(0, 0, 1200, 800, RGB(255, 255, 255))
       Box(WindowMouseX(0), WindowMouseY(0), 80, 30, RGB(0, 0, 0))
       Box(WindowMouseX(0)+2, WindowMouseY(0)+2, 80-4, 30-4, RGB(255, 255, 255))
      
        draw_mnemoshem()
        
       StopDrawing()
       out_thread=0
        Delay(time_pause)
     ForEver
     
     
   Case 3
     
       Repeat
       StartDrawing(CanvasOutput(#canvas))
       out_thread=1
       Box(0, 0, 1200, 800, RGB(255, 255, 255))

       Circle(WindowMouseX(0), WindowMouseY(0), 15,RGB(0,0,0))
       Circle(WindowMouseX(0), WindowMouseY(0), 12,RGB(255,0,0))
       draw_mnemoshem()
       
       StopDrawing()
       
       out_thread=0
       Delay(time_pause)
  
     ForEver
   Case 4
     Repeat
       StartDrawing(CanvasOutput(#canvas))
       out_thread=1
       Box(0, 0, 1200, 800, RGB(255, 255, 255))
       
       Box(WindowMouseX(0),WindowMouseY(0), 400, 200, RGB(0, 0, 0))
       Box(WindowMouseX(0)+2,WindowMouseY(0)+2, 400-4, 200-4, RGB(255, 255, 255))
          
          
          draw_mnemoshem()
          StopDrawing()
          
             out_thread=0
             Delay(time_pause)
       ForEver
     EndSelect
EndProcedure 
   
Procedure pusk_panel() 
  OpenWindow(#window_pusk, 0, 0, 1200, 800, "параметры элемента" ) 
  CanvasGadget(#canvas_pusk, 0, 0, 1200, 800) 
  
  EndProcedure
  
Procedure pusc_mnemoshem(q)
      
  Repeat
  
      StartDrawing(CanvasOutput(#canvas_pusk))
      For i = 1 To count_element_mnemoshem
        Select  mnemo(i)\type
          Case 1
              count_nadpis_mnemoshem=count_nadpis_mnemoshem+1
              DrawText(mnemo(i)\positionX,mnemo(i)\positionY,nad.pis(count_nadpis_mnemoshem)\text)
           Case 2
             count_okno_dannih_mnemoshem=count_okno_dannih_mnemoshem+1

               Box(mnemo(i)\positionX, mnemo(i)\positionY, 80, 30, RGB(0, 0, 0))
               Box(mnemo(i)\positionX+2, mnemo(i)\positionY+2, 80-4, 30-4, RGB(255, 255, 255))
                tag_number=dannie(count_okno_dannih_mnemoshem)\tag
               DrawText(mnemo(i)\positionX+35,mnemo(i)\positionY+7,Str(oprosm(tag_number)),RGB(0,0,0),RGB(255,255,255))
               
           Case 3
             count_discret_input_mnemoshem=count_discret_input_mnemoshem+1
             bit=discret(count_discret_input_mnemoshem)\bit+1
             
           If discret_oprosm(bit,count_discret_input_mnemoshem)=0
             Circle(mnemo(i)\positionX, mnemo(i)\positionY, 15,RGB(0,0,0))
             Circle(mnemo(i)\positionX, mnemo(i)\positionY, 12,RGB(255,0,0))
           EndIf  
           
            If discret_oprosm(bit,count_discret_input_mnemoshem)=1
             Circle(mnemo(i)\positionX, mnemo(i)\positionY, 15,RGB(0,0,0))
             Circle(mnemo(i)\positionX, mnemo(i)\positionY, 12,RGB(0,255,0))
           EndIf
           
         Case 4 
           count_graf_mnemoshem=count_graf_mnemoshem+1
           tag_number=grafik(count_graf_mnemoshem)\tag
         graf(mnemo(i)\positionX ,mnemo(i)\positionY,oprosm(tag_number))    
          EndSelect
        Next
      
        count_nadpis_mnemoshem=0
        count_okno_dannih_mnemoshem=0
        count_discret_input_mnemoshem=0
        count_graf_mnemoshem=0
         StopDrawing()
       Delay(1000)
        ForEver
EndProcedure
  
Procedure menu()
  
  If CreateMenu(0, WindowID(0))
    MenuTitle("Файл")
      MenuItem( 1, "Новый проект")
      MenuItem( 2, "Открыть")
      MenuItem( 3, "Сохранить")
      MenuItem( 4, "Сохранить как..")
      MenuItem( 5, "Сохранить отчет в Excel")
      
      
      MenuTitle("Основное")
      MenuItem( #pusk, "Пуск")
      MenuItem( #menu_connection, "Создать соединение")
      MenuItem( #menu_indow_connection, "Просмотреть соединения")
      MenuItem( #menu_tag, "Создать тэг")
      MenuItem(#menu_indow_tag,"Просмотреть тэги")
      
      
      
      MenuTitle("Мнемосхема")
      MenuItem(#add_node,"Надпись")
      MenuItem(#menu_okno_dannyh,"Аналоговый вход")
      MenuItem(#menu_okno_discret,"Дискретный вход")
      MenuItem(#menu_okno_graf,"График")
    EndIf
    
    

  EndProcedure
  
Procedure panel_connect()
    
    OpenWindow(#panel_connect, 10, 100, 600, 800, "Cписок соединений" ,#PB_Window_SystemMenu | #PB_Window_MinimizeGadget | #PB_Window_MaximizeGadget)
    ListIconGadget(#table_connection, 50,  50, 500, 600, "№", 60, #PB_ListIcon_GridLines)
          AddGadgetColumn(#table_connection, 2, "название", 100)
          AddGadgetColumn(#table_connection, 3, "адрес", 100)
          AddGadgetColumn(#table_connection, 4, "порт", 100)
          
          For i=1 To count_create_port-1
          AddGadgetItem(#table_connection, i, Str(i)+Chr(10)+connect(i)\name_connection+Chr(10)+connect(i)\ip+Chr(10)+connect(i)\tcp)
    Next
EndProcedure
  
Procedure panel_tag()
    OpenWindow(#panel_tag, 10, 10, 800, 600, "Cписок тегов" ,#PB_Window_SystemMenu | #PB_Window_MinimizeGadget | #PB_Window_MaximizeGadget)
    
    ListIconGadget(#table_tag, 10,  10, 750, 500, "№", 50, #PB_ListIcon_GridLines)
          
          AddGadgetColumn(#table_tag, 1, "название", 100)
          AddGadgetColumn(#table_tag, 2, "адрес", 100)
          AddGadgetColumn(#table_tag, 3, "функция", 100)
          AddGadgetColumn(#table_tag, 4, "регистр", 100)
          AddGadgetColumn(#table_tag, 5, "длина", 100)
          AddGadgetColumn(#table_tag, 6, "соединение", 100)
           AddGadgetColumn(#table_tag, 7, "тип данных", 100)
    For n=1 To count_create_tag-1
    number_connection= modbus(n)\number_connection
      
    name_connection.s=connect(number_connection)\name_connection
    
    start_reg_temp.s=modbus(n)\start_reg_hi+"  "+modbus(n)\start_reg_lo
    leght_temp.s=modbus(n)\leght_hi+"  "+modbus(n)\leght_lo
    
    
    AddGadgetItem(#table_tag, n, Str(n)+Chr(10)+modbus(n)\name_tag+Chr(10)+modbus(n)\number_divice+Chr(10)+modbus(n)\function+Chr(10)+ start_reg_temp+Chr(10)+leght_temp+Chr(10)+name_connection+Chr(10)+type_dannih(modbus(n)\type))
    Next
EndProcedure
  
  
Procedure panel()
CanvasGadget(#canvas, 0, 0, 1200, 800)           
EditorGadget(#position_mouse,900,700,100,20)
   
  EndProcedure
  


Procedure parametr_port()
  OpenWindow( #window_create_port, 100, 100, 400, 200, "Cоздать соединения" ,#PB_Window_SystemMenu | #PB_Window_MinimizeGadget | #PB_Window_MaximizeGadget)
  
    TextGadget(#text_name_connection,10,10,200,20,"название соединения")
    EditorGadget(#edit_name_connection,210,10,120,20)
    
    TextGadget(#text_IP_tag,10,40,200,20,"IP adress")
    IPAddressGadget(#edit_IP_tag,210,40,120,20)
    
    TextGadget(#text_tcp_port,10,70,200,20,"TCP порт")
    EditorGadget(#gadget_tcp_port, 210, 70, 80, 20)
        
    ButtonGadget(#create_port,10,150,100,30,"создать")
  EndProcedure
  
Procedure create_port()
    
  
  n=count_create_port
  
  connect(n)\tcp=Val(GetGadgetText(#gadget_tcp_port))
  connect(n)\ip=GetGadgetText(#edit_IP_tag)
  connect(n)\name_connection=GetGadgetText(#edit_name_connection)
  
  flag=create_connection(connect(n)\tcp,connect(n)\ip,n) 
  If flag=1
    ;AddGadgetItem(#table_connection, n, Str(n)+Chr(10)+connect(n)\name_connection+Chr(10)+connect(n)\ip+Chr(10)+connect(n)\tcp)
    count_create_port=count_create_port+1
  Else 
    
    EndIf
EndProcedure
  
Procedure parametr_tag()
  OpenWindow( #window_create_tag, 100, 100, 400, 600, "Создать соединения" ,#PB_Window_SystemMenu | #PB_Window_MinimizeGadget | #PB_Window_MaximizeGadget)
  
    TextGadget(#text_name_tag,10,10,200,20,"название тега")
    EditorGadget(#edit_name_tag,210,10,120,20)
    
    TextGadget(#text_connection_tag,10,40,200,20,"соединение")
    
    ComboBoxGadget(#edit_connection_tag,210,40,120,20) 
    For i = 1 To count_create_port-1        
       AddGadgetItem(#edit_connection_tag, -1,connect(i)\name_connection)
      Next
      
      TextGadget(#text_number_device_tag,10,70,200,20,"номер устройства")
      SpinGadget(#edit_number_device_tag, 210,70,120,20, 0, 255)
      
      TextGadget(#text_function_tag,10,100,200,20,"функция")
      
      ComboBoxGadget(#edit_function_tag,210,100,120,20) 
    For i = 1 To 6       
       AddGadgetItem(#edit_function_tag, -1,"0x0"+Str(i))
     Next
     
      
      TextGadget(#text_registr_tag,10,130,200,20,"адрес регистра")
      EditorGadget(#edit_registr_tag_hi,210,130,50,20)
      EditorGadget(#edit_registr_tag_lo,280,130,50,20)
      
      TextGadget(#text_lenght_tag,10,160,200,20,"кол-во регистров")
      EditorGadget(#edit_lenght_tag_hi,210,160,50,20)
      EditorGadget(#edit_lenght_tag_lo,280,160,50,20)
      
       ComboBoxGadget(#edit_type_tag,210,190,120,20) 
       TextGadget(#text_type_tag,10,190,120,20,"тип данных")   
       AddGadgetItem(#edit_type_tag, -1,"BOOL")
       AddGadgetItem(#edit_type_tag, -1,"INT")
       AddGadgetItem(#edit_type_tag, -1,"REAL")
        
        
    ButtonGadget(#create_tag,10,500,100,30,"создать")
EndProcedure
  
Procedure create_tag()
    
     n=count_create_tag
    modbus(n)\name_tag=GetGadgetText(#edit_name_tag)
    number_divice.s=GetGadgetText(#edit_number_device_tag)
    modbus(n)\function=Right(GetGadgetText(#edit_function_tag),2)
    modbus(n)\start_reg_hi=GetGadgetText(#edit_registr_tag_hi)
    modbus(n)\start_reg_lo=GetGadgetText(#edit_registr_tag_lo)
    modbus(n)\leght_hi=GetGadgetText(#edit_lenght_tag_hi)
    modbus(n)\leght_lo=GetGadgetText(#edit_lenght_tag_lo)
    modbus(n)\type=GetGadgetState(#edit_type_tag)+1
    
    modbus(n)\number_connection=GetGadgetState(#edit_connection_tag)+1
    
    If Len(number_divice)=1
      modbus(n)\number_divice="0"+ number_divice
    Else
       modbus(n)\number_divice=number_divice
    EndIf
    
      ;number_connection=GetGadgetState(#edit_connection_tag)+1
     
    count_create_tag=count_create_tag+1
    
EndProcedure
    
Procedure opros(q)
  Repeat
    For i= 1 To count_okno_dannih
      
      number_tag=dannie(i)\tag
     
      device.s=modbus(number_tag)\number_divice
      functia.s=modbus(number_tag)\function
      reg_hi.s=modbus(number_tag)\start_reg_hi
      reg_lo.s=modbus(number_tag)\start_reg_lo
      leght_hi.s=modbus(number_tag)\leght_hi
      leght_lo.s=modbus(number_tag)\leght_lo
      connect=modbus(number_tag)\number_connection
      
      oprosm(i)= modbus_tcp(device,functia,reg_hi,reg_lo,leght_hi,leght_lo,connect)
      
    Next
    
    For i= 1 To count_discret_input
      
      number_tag=discret(i)\tag
      bit=discret(i)\bit
      device.s=modbus(number_tag)\number_divice
      functia.s=modbus(number_tag)\function
      reg_hi.s=modbus(number_tag)\start_reg_hi
      reg_lo.s=modbus(number_tag)\start_reg_lo
      leght_hi.s=modbus(number_tag)\leght_hi
      leght_lo.s=modbus(number_tag)\leght_lo
      connect=modbus(number_tag)\number_connection
      
      ;Debug number_tag
      ;Debug  device+" "+functia+" "+reg_hi+" "+reg_lo+" "+leght_hi+" "+leght_lo+" "+connect
      For j=1 To 8
        If modbus_tcp(device,functia,reg_hi,reg_lo,leght_hi,leght_lo,connect) & j=j
          discret_oprosm(j,i)=1
        ElseIf modbus_tcp(device,functia,reg_hi,reg_lo,leght_hi,leght_lo,connect) & j=0
          discret_oprosm(j,i)=0
        EndIf
        
        
      Next
      
      
     
    Next
    
    Delay(1000)
    ForEver
EndProcedure

  
Procedure sav_parametr_element()
  n=count_element_mnemoshem
  mnemo(n)\positionX=WindowMouseX(0)
  mnemo(n)\positionY=WindowMouseY(0)
  mnemo(n)\type=type_element
  count_element_mnemoshem=count_element_mnemoshem+1
EndProcedure 
      

  


    
    
Procedure graf(x,y,z)
  size_y=200
  koef_y.f=size_y/210
  
  size_x=400
  koef_x.f=size_x/600
  
      steps=30
      one_step=20*koef_x.f
      
      
      time_step=2
      min=0
      max=200
      
     z=50
      
   
      ;Repeat
        
        
        If Second(Date())%time_step=0
          time(steps+1)=FormatDate("%hh:%ii:%ss", Date()) 
        
          
        Graph(steps+1)=150-150*z/max
        
        For i=0 To steps
          Graph(i)=Graph(i+1)
          
           time(i)=time(i+1)
          
          
          Next 
     
   
     ;If StartDrawing(CanvasOutput(#canvas))
          Box(x, y, 600*koef_x.f+50, 210*koef_y, RGB(255, 255, 255))
          For i=0 To 5
            DrawText(x-20,y+135*koef_y-(i)*30*koef_y,Str(max/5*i), RGB(0, 0, 0), RGB(255, 255, 255))
            LineXY(x, y+150*koef_y-(i)*30*koef_y, x+600*koef_x+20, y+150*koef_y-(i)*30*koef_y, RGB(200, 200, 200))
            Next 
            For i=1 To steps
              LineXY(i*one_step+x, Graph(i-1)*koef_y+y, (i+1)*one_step+x, Graph(i)*koef_y+y, RGB(0, 0, 0))
              
              DrawRotatedText((i-2)*one_step+x, 200*koef_y+y, time(i), 90, RGB(0, 0, 0))
              ;If time(i)<>""
              LineXY(i*one_step+x, 0+y, i*one_step+x, 160*koef_y+y, RGB(200, 200, 200))
           
             Next
    
             
          EndIf  

      
  
  
  
      EndProcedure



 

   
   
  
 Procedure action()
    
     Select WaitWindowEvent()

      Case #PB_Event_Menu;если выбрано меню

          Select EventMenu()  ; To see which menu has been selected
             Case #menu_connection
               parametr_port()
             Case #menu_tag
               parametr_tag()
               
              Case #add_node
                parametr_element(1)
              Case #menu_okno_dannyh
                parametr_element(2)
              Case #menu_okno_discret
                parametr_element(3)
                Case #menu_okno_graf
                parametr_element(4)
              Case #pusk
                pusk_panel()
                 ;CreateThread(@opros(),1)
                  thread_pusk=CreateThread(@pusc_mnemoshem(),1)
                
              Case #menu_indow_connection
                panel_connect()
                Case #menu_indow_tag
                  panel_tag()
                
          EndSelect
        
       Case #PB_Event_Gadget;если выбрать гаджет
          
         Select EventGadget()  ; To see which menu has been selected
             
             
           ;Case #button_parametr_node
            ; create_node()
           Case #button_create_nadpis
             save_parametr(1)
             Delay(100)
             thread=CreateThread(@vibor_pozicii(),1)
             CloseWindow(#window_create_nadpis)
             
           Case #button_create_okno_dannih
             save_parametr(2)
              Delay(100)
             thread=CreateThread(@vibor_pozicii(),2)
             CloseWindow(#window_create_okno_dannih)
             
           Case  #button_create_discret
             save_parametr(3)
              Delay(100)
             thread=CreateThread(@vibor_pozicii(),3)
             CloseWindow(#window_create_discret)
             
             
             Case  #button_create_graf
             save_parametr(4)
              Delay(100)
             thread=CreateThread(@vibor_pozicii(),4)
             CloseWindow(#window_create_graf)
             
           Case #nadpis_color
             font(type_element,1)
             Case #nadpis_fon
             font(type_element,0)
           Case #add_port
             parametr_port()
           Case #create_port
             create_port()
             CloseWindow(#window_create_port)
           Case #add_tag
             parametr_tag()
           Case #create_tag
             create_tag()
             CloseWindow(#window_create_tag)             
           Case #edit_number_device_tag
             SetGadgetText(#edit_number_device_tag, Str(GetGadgetState(#edit_number_device_tag)))
             
              Case #spin_byte_number
                SetGadgetText(#spin_byte_number, Str(GetGadgetState(#spin_byte_number)))
                
             Case #canvas
               Select EventType()
                 Case #PB_EventType_LeftClick 
                   If thread > 0 And out_thread=0
                     
                     sav_parametr_element()
                     
                    
                     KillThread(thread)
                     
                   
                     thread=0
                     

                   EndIf
                   
                   ;Case  #PB_EventType_LeftDoubleClick 
                   ;If thread > 0  
                     
                     ;sav_parametr_element()
                     ;Delay(10)
                     
                     ;KillThread(thread)
                  
                     ;Delay(10)
                     ;;StopDrawing()
                     ;Delay(10)
                     ;thread=0
                     

                     ;EndIf
                 Case #PB_EventType_RightClick       : ;Debug "Click with right mouse button"
               
                 Case #PB_EventType_RightDoubleClick : ;Debug "Double-click with right mouse button"
               EndSelect
               
               
          EndSelect    
             
          
      Case #PB_Event_CloseWindow
        Select EventWindow()  ; To see which menu has been selected

          Case 0
            CloseWindow(0)
            End
            
          Case #window_parametr_node
            CloseWindow(#window_parametr_node)
          
          Case #window_create_port
            CloseWindow(#window_create_port)
          Case(#window_create_tag)
            CloseWindow(#window_create_tag)
           Case(#panel_connect)
            CloseWindow(#panel_connect) 
          Case(#panel_tag)
            CloseWindow(#panel_tag)
          Case #window_create_nadpis
            CloseWindow(#window_create_nadpis)
          Case #window_create_okno_dannih
            CloseWindow(#window_create_okno_dannih)
          Case  #window_pusk
            
            KillThread(thread_pusk)
            
            CloseWindow(#window_pusk)
        EndSelect

    EndSelect

    EndProcedure
 
 
 
If OpenWindow(0, 100, 150, 1200, 800, "SinApp" ,#PB_Window_SystemMenu | #PB_Window_MinimizeGadget | #PB_Window_MaximizeGadget)
  
  
  panel()
  menu()
  CreateThread(@opros(),1)
  
  Repeat
    
    
   
     
    
    
    action()
    
  
 
  ;SetGadgetText(#position_mouse,"x="+Str(WindowMouseX(0))+"  y="+Str(WindowMouseY(0)))
  ForEver 
  

EndIf

; IDE Options = PureBasic 5.60 (Windows - x64)
; CursorPosition = 53
; FirstLine = 33
; Folding = AAA7
; EnableXP