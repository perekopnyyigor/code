IncludeFile "head.pb"

Enumeration massiv
  #del_tovar
  #del_provider
  #sel_provider
  #sel_tovar
  EndEnumeration
;--------------------------------------------------------------------------------------------------

Global Dim number.s(10,1000)



 ExamineDesktops()
If OpenWindow(#main_window, 0, 0, 1200, 800, "SinApp" ,#PB_Window_SystemMenu | #PB_Window_MaximizeGadget|#PB_Window_Maximize| #PB_Window_MinimizeGadget)

menu()
connection_db()  
Repeat
action()
ForEver
EndIf
End  
;----------------------------------------------------------------------------------------------------
Procedure connection_db()
  UseSQLiteDatabase()
  DatabaseFile$ = GetTemporaryDirectory()+"Database6.sqlite"
  If OpenDatabase(0, DatabaseFile$, "", "")
    
    
    
  MessageRequester("Message","database open")
  Else
  
    CreateFile(0, DatabaseFile$)
    CloseFile(0)
    OpenDatabase(0, DatabaseFile$, "", "")
    DatabaseUpdate(0, "CREATE TABLE tovar (name CHAR(50), articl INTEGER NOT NULL, price INT,zakup INT, PRIMARY KEY(articl))")
    DatabaseUpdate(0, "CREATE TABLE provider (id INTEGER NOT NULL, name CHAR(50), tel CHAR(50), inn CHAR(50), PRIMARY KEY(id))")
    MessageRequester("Message","database create")
  EndIf
  

  
EndProcedure


Procedure action()
  
    
    
    
     Select WaitWindowEvent()

      Case #PB_Event_Menu

          Select EventMenu()  ; To see which menu has been selected
            Case #menu_tovar :panel_tovar()
            Case #menu_provider :panel_provider()
              Case #menu_zakup :panel_zacup()
          EndSelect
        
       Case #PB_Event_Gadget
          Select EventGadget()  ; To see which menu has been selected
            Case #button_parametr_tovar :panel_parametr_tovar()
            Case #button_del_tovar:del_tovar()
            Case #button_parametr_provider :panel_parametr_provider() 
            Case #button_del_provider:del_provider() 
            Case #button_parametr_zacup :panel_parametr_zacup() 
            Case #button_select_provider: panel_select_provider()
            Case #button_search_tovar:db_search_tovar() 
            Case #button_search:db_search_provider()  
            Case #button_select_tovar: panel_select_tovar() 
              
            Case #button_add_sel_tovar
              select_tovar()
              CloseWindow(#window_select_tovar)
            
            Case #button_add_tovar 
              add_tovar()
              CloseWindow(#window_parametr_tovar)
              
            Case #button_select:select_provider()
              CloseWindow(#window_select_provider)  
             Case #button_add_provider
              add_provider()
              CloseWindow(#window_parametr_provider)
          EndSelect
          
      Case #PB_Event_CloseWindow
        Select EventWindow()    
            
          Case #main_window
            CloseWindow(#main_window)
            End
           
          Case #window_tovar: CloseWindow(#window_tovar)    
          Case #window_parametr_tovar: CloseWindow(#window_parametr_tovar)
          Case #window_provider: CloseWindow(#window_provider)     
          Case #window_zacup: CloseWindow(#window_zacup) 
          Case #window_select_provider: CloseWindow(#window_select_provider) 
          Case #window_parametr_zacup: CloseWindow(#window_parametr_zacup)
          Case #window_select_tovar: CloseWindow(#window_select_tovar)
        EndSelect
        
     
    EndSelect
    
     

 

  EndProcedure
    
  Procedure menu()
    
  
  
  If CreateMenu(0, WindowID(0))
    MenuTitle("Касса")
      MenuItem( #menu_kassa_prihod, "Приход")
      MenuItem( #menu_kassa_rashod, "Расход")
      
      
    MenuTitle("Торговля")
      MenuItem( #menu_zakup, "Закуп")
      MenuItem( #menu_prodazha, "Продажа")
      
    MenuTitle("Добавить")
    MenuItem( #menu_tovar, "Товар")
    MenuItem( #menu_provider, "Поставщик")
    EndIf
    
  EndProcedure  
  
 ;--------------------------------------Товар 
  
Procedure panel_tovar()
    
    
      
    OpenWindow(#window_tovar, 0, 0, 800, 800, "Список товара" ,#PB_Window_SystemMenu|#PB_Window_WindowCentered, WindowID(#main_window))
    
    
    ListIconGadget(#table_tovar, 10,  10, 650, 500, "Наименование", 160, #PB_ListIcon_GridLines|#PB_ListIcon_FullRowSelect)
    AddGadgetColumn(#table_tovar, 1, "Артикул", 160)
    AddGadgetColumn(#table_tovar, 2, "Розничная цена", 160)
    AddGadgetColumn(#table_tovar, 3, "Закупочная цена", 160)
          
      ButtonGadget(#button_parametr_tovar,10,600,100,30,"добавить товар")    
      ButtonGadget(#button_del_tovar,10,640,100,30,"удалить товар") 
      
    db_query_tovar()  
    EndProcedure
  
Procedure panel_parametr_tovar()
      

      OpenWindow(#window_parametr_tovar, 0, 0, 400, 300, "Добавить товар" ,#PB_Window_SystemMenu|#PB_Window_WindowCentered, WindowID(#window_tovar))
       
      TextGadget(#text_name_tovar,10,10,100,20,"Наименование")
      EditorGadget(#edit_name_tovar,120,10,100,20)
      
      TextGadget(#text_price_tovar,10,40,100,20,"Розничная цена")
      EditorGadget(#edit_price_tovar,120,40,100,20)
      
      TextGadget(#text_zakup_price_tovar,10,70,100,20,"Закупочная цена")
      EditorGadget(#edit_zakup_price_tovar,120,70,100,20)
      
      ButtonGadget(#button_add_tovar,10,200,100,30,"добавить")
      EndProcedure
  
  Procedure add_tovar()
    name.s=GetGadgetText(#edit_name_tovar)
    price.f=ValF(GetGadgetText(#edit_price_tovar))
    zacup.f=ValF(GetGadgetText(#edit_zakup_price_tovar))
    
    DatabaseUpdate(0, "INSERT INTO tovar (name,  price, zakup) VALUES ('"+name+"', '"+StrF(price,2)+"','"+StrF(zacup,2)+"')")

    db_query_tovar()  
 
  EndProcedure
 
Procedure db_query_tovar()  
  DatabaseQuery(0, "SELECT * FROM tovar ")
  Protected count=1
  Protected article.s
  
    ClearGadgetItems(#table_tovar)
    
    While NextDatabaseRow(0) 
      article=GetDatabaseString(0, 1)
           AddGadgetItem(#table_tovar, count, GetDatabaseString(0, 0)+Chr(10)+article+Chr(10)+GetDatabaseString(0, 2)+Chr(10)+GetDatabaseString(0, 3))
           number(#del_tovar,count)=article
    
           count=count+1
         Wend 
         
EndProcedure  

Procedure del_tovar()
  k=GetGadgetState(#table_tovar)+1
  del.s=number(#del_tovar,k)
 DatabaseUpdate(0, "DELETE  FROM tovar WHERE articl='"+del+"'")
 db_query_tovar() 
 
 If k<1
   MessageRequester("info","Товар не выбран")
 EndIf
 
  EndProcedure
  
 ;--------------------------------------Поставщики
  
 Procedure panel_provider()
    
    
    
      
    OpenWindow(#window_provider, 0, 0, 800, 800, "Список поставщиков" ,#PB_Window_SystemMenu|#PB_Window_WindowCentered, WindowID(#main_window))
    
    
    ListIconGadget(#table_provider, 10,  10, 650, 500, "Наименование", 160, #PB_ListIcon_GridLines|#PB_ListIcon_FullRowSelect)
    AddGadgetColumn(#table_provider, 1, "Телефон", 160)
    AddGadgetColumn(#table_provider, 2, "ИНН", 160)
   
          
      ButtonGadget(#button_parametr_provider,10,600,100,30,"добавить ")    
      ButtonGadget(#button_del_provider,10,640,100,30,"удалить ") 
      
     db_query_provider() 
    EndProcedure
    
 Procedure panel_parametr_provider()
      
      

      OpenWindow(#window_parametr_provider, 0, 0, 400, 300, "Добавить товар" ,#PB_Window_SystemMenu|#PB_Window_WindowCentered, WindowID(#main_window))
       
      TextGadget(#text_name_provider,10,10,100,20,"Наименование")
      EditorGadget(#edit_name_provider,120,10,100,20)
      
      TextGadget(#text_tel_provider,10,40,100,20,"Телефон")
      EditorGadget(#edit_tel_provider,120,40,100,20)
      
      TextGadget(#text_inn_provider,10,70,100,20,"ИНН")
      EditorGadget(#edit_inn_provider,120,70,100,20)
      
      ButtonGadget(#button_add_provider,10,200,100,30,"добавить")
    EndProcedure
    
 Procedure add_provider()
      
      
    name.s=GetGadgetText(#edit_name_provider)
    tel.s=(GetGadgetText(#edit_tel_provider))
    inn.s=(GetGadgetText(#edit_inn_provider))
    
    If DatabaseUpdate(0, "INSERT INTO provider (name,  tel, inn) VALUES ('"+name+"', '"+tel+"','"+inn+"')")
      MessageRequester("","ok")
    Else
      MessageRequester("","bad")
      EndIf 
    db_query_provider() 
 
  EndProcedure
   
 Procedure db_query_provider()  
    
   DatabaseQuery(0, "SELECT * FROM provider ")
   
  Protected count=1
  Protected article.s
  
    ClearGadgetItems(#table_provider)
    
    While NextDatabaseRow(0) 
        article=GetDatabaseString(0, 0)
           AddGadgetItem(#table_provider, count, GetDatabaseString(0, 1)+Chr(10)+GetDatabaseString(0, 2)+Chr(10)+GetDatabaseString(0, 3))
           number(#del_provider,count)=article
    
           count=count+1
         Wend
        
       EndProcedure 
       
 Procedure del_provider()
  k=GetGadgetState(#table_provider)+1
  del.s=number(#del_provider,k)
 DatabaseUpdate(0, "DELETE  FROM provider WHERE id='"+del+"'")
 db_query_provider() 
 
 If k<1
   MessageRequester("info","Поставщик не выбран")
 EndIf
 
  EndProcedure
  ;--------------------------------------Закуп
  
 Procedure panel_zacup()

    OpenWindow(#window_zacup, 0, 0, 800, 800, "Приход товара" ,#PB_Window_SystemMenu|#PB_Window_WindowCentered, WindowID(#main_window))

    ListIconGadget(#table_zacup, 10,  10, 650, 500, "Дата", 160, #PB_ListIcon_GridLines|#PB_ListIcon_FullRowSelect)
    AddGadgetColumn(#table_zacup, 1, "Поставщик", 160)
    AddGadgetColumn(#table_zacup, 2, "Сумма", 160)
  
      ButtonGadget(#button_parametr_zacup,10,600,100,30,"добавить ")    
      ButtonGadget(#button_del_zacup,10,640,100,30,"удалить ") 
    EndProcedure
    
 Procedure panel_parametr_zacup()
  
      OpenWindow(#window_parametr_zacup, 0, 0, 600, 400, "Приход товара" ,#PB_Window_SystemMenu|#PB_Window_WindowCentered, WindowID(#window_zacup))
       
      TextGadget(#text_data_zacup,10,10,100,20,"Дата")   
      DateGadget(#edit_data_zacup, 120, 10, 210, 25, "Date: %mm/%dd/%yyyy Time: %hh:%ii")
      
      TextGadget(#text_provider_zacup,10,50,100,20,"Поставщик")
      EditorGadget(#edit_provider_zacup,120,50,100,20)
      ButtonGadget(#button_select_provider,250,50,60,20,"Выбрать") 
      
      TextGadget(#text_provider_tovar,10,90,100,20,"Товар")
      ButtonGadget(#button_select_tovar,250,90,60,20,"Выбрать")
      
      ListIconGadget(#table_tovar_zakup, 10,  120, 550, 190, "Товар", 150, #PB_ListIcon_GridLines|#PB_ListIcon_FullRowSelect)
      AddGadgetColumn(#table_tovar_zakup, 1, "Цена", 130)
      AddGadgetColumn(#table_tovar_zakup, 2, "Количество", 130)
      AddGadgetColumn(#table_tovar_zakup, 3, "Стоимость", 130)
      
      ButtonGadget(#button_add_zacup,10,320,100,30,"добавить")
    EndProcedure
   ;--------------------------------------Закуп выбор поставщика 
 Procedure panel_select_provider()
  
      OpenWindow(#window_select_provider, 0, 0, 400, 300, "Выбрать поставщика" ,#PB_Window_SystemMenu|#PB_Window_WindowCentered, WindowID(#window_parametr_zacup))
      EditorGadget(#edit_select_provider,10,10,200,20)
      ListIconGadget(#table_select_provider, 10,  50, 350, 190, "Наименование", 345, #PB_ListIcon_GridLines|#PB_ListIcon_FullRowSelect)
       ButtonGadget(#button_search,220,10,80,20,"Поиск")
      ButtonGadget(#button_select,10,250,100,30,"Выбрать")
      db_select_provider() 
      
    EndProcedure
    
 Procedure db_select_provider()  
      
    
   DatabaseQuery(0, "SELECT id,name FROM provider ")
   
  
  
    ClearGadgetItems(#table_select_provider)
    
    While NextDatabaseRow(0) 
      article.s=GetDatabaseString(0, 0)
      
        AddGadgetItem(#table_select_provider, count, GetDatabaseString(0, 1))
        
           number(#sel_provider,count)=article
    
           count=count+1
         Wend
        
       EndProcedure 
       
 Procedure db_search_provider()  
   ClearGadgetItems(#table_select_provider)   
   provider.s=GetGadgetText(#edit_select_provider)
   
   If DatabaseQuery(0, "SELECT id,name FROM provider WHERE name LIKE '"+provider+"%' ")
     
   EndIf
   While NextDatabaseRow(0) 
   article.s=GetDatabaseString(0, 0)
      
        AddGadgetItem(#table_select_provider, count, GetDatabaseString(0, 1))
        
           number(#sel_provider,count)=article
    
           count=count+1
         Wend
        
       EndProcedure 
       
 Procedure select_provider()
         
  k=GetGadgetState(#table_select_provider)
  del.s=number(#sel_provider,k)
  DatabaseQuery(0, "SELECT name  FROM provider WHERE id='"+del+"'")
  
    While NextDatabaseRow(0) 
      name.s=GetDatabaseString(0, 0)
      
        
         Wend
 

         ClearGadgetItems(#edit_provider_zacup)
         SetGadgetText(#edit_provider_zacup,name)

 
       EndProcedure
 ;--------------------------------------Закуп выбор товара    
 Procedure panel_select_tovar()
  
      OpenWindow(#window_select_tovar, 0, 0, 400, 350, "Выбрать товар" ,#PB_Window_SystemMenu|#PB_Window_WindowCentered, WindowID(#window_parametr_zacup))
      EditorGadget(#edit_select_tovar,10,10,200,20)
      ListIconGadget(#table_select_tovar, 10,  50, 350, 190, "Наименование", 345, #PB_ListIcon_GridLines|#PB_ListIcon_FullRowSelect)
      ButtonGadget(#button_search_tovar,220,10,80,20,"Поиск")
      TextGadget(#text_quantity_tovar,10,253,100,20,"Количество")
      EditorGadget(#edit_quantity_tovar,120,250,150,20)
      ButtonGadget(#button_add_sel_tovar,10,300,100,30,"Выбрать")
      db_select_tovar()  
      
    EndProcedure
    
 Procedure db_select_tovar()  
   count=0   
    
   DatabaseQuery(0, "SELECT articl, name FROM tovar ")
   
  
  
    
    
    While NextDatabaseRow(0) 
      article.s=GetDatabaseString(0, 0)
     
        AddGadgetItem(#table_select_tovar, count, GetDatabaseString(0, 1))
        
           number(#sel_tovar,count)=article
    
           count=count+1
         Wend
       
     EndProcedure 
     
 Procedure select_tovar()
       count=0
         
       k=GetGadgetState(#table_select_tovar)
       
       del.s=number(#sel_tovar,k)
       
       DatabaseQuery(0, "SELECT name, zakup  FROM tovar WHERE articl='"+del+"'")
  
    While NextDatabaseRow(0) 
      name.s=GetDatabaseString(0, 0)
      price.s=GetDatabaseString(0, 1)
      quantity.s=GetGadgetText(#edit_quantity_tovar) 
      summ=ValF(price)*ValF(quantity)
         Wend

         AddGadgetItem(#table_tovar_zakup,1, name +Chr(10)+price+Chr(10)+quantity+Chr(10)+Str(summ))

       EndProcedure
       
       
 Procedure db_search_tovar()  
         count=0
   ClearGadgetItems(#table_select_tovar)   
   tovar.s=GetGadgetText(#edit_select_tovar)
   
   If DatabaseQuery(0, "SELECT articl,name FROM tovar WHERE name LIKE '"+tovar+"%' ")
     
   EndIf
   While NextDatabaseRow(0) 
   article.s=GetDatabaseString(0, 0)
      
        AddGadgetItem(#table_select_tovar, count, GetDatabaseString(0, 1))
        
           number(#sel_tovar,count)=article
    
           count=count+1
         Wend
        
       EndProcedure
; IDE Options = PureBasic 5.60 (Windows - x64)
; CursorPosition = 215
; FirstLine = 81
; Folding = CAAg
; EnableXP