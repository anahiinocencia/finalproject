#Anahí Inocencia Arana Soto
library(shiny)
library(shinydashboard)
library(DBI)
library(RPostgres)
library(dplyr)
library(DT)
library(ggplot2)
library(forcats)

# Conexión a base de datos
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "Northwind",
  host = "localhost",
  port = 5433,
  user = "demouser",
  password = "123"
)

# Cargar datos
customers <- tbl(con, "customers") %>% collect()
orders <- tbl(con, "orders") %>% collect()
products <- tbl(con, "products") %>% collect()
order_details <- tbl(con, "order_details") %>% collect()
categories <- tbl(con, "categories") %>% collect()
employees <- tbl(con, "employees") %>% collect()
territories <- tbl(con, "territories") %>% collect()

order_details <- order_details %>%
  mutate(monto = unit_price * quantity * (1 - discount))

# Ventas por país
ventas_pais_monto <- order_details %>%
  left_join(orders, by = "order_id") %>%
  left_join(customers, by = "customer_id") %>%
  mutate(country = ifelse(is.na(country), "Desconocido", country)) %>%
  group_by(country) %>%
  summarise(total_monto = sum(monto, na.rm = TRUE)) %>%
  slice_max(total_monto, n = 12)

ventas_pais <- orders %>%
  left_join(customers, by = "customer_id") %>%
  mutate(country = ifelse(is.na(country), "Desconocido", country)) %>%
  count(country, name = "total_orders") %>%
  slice_max(total_orders, n = 12)

# Ventas por categoría
ventas_categoria_monto <- order_details %>%
  left_join(products, by = "product_id") %>%
  left_join(categories, by = "category_id") %>%
  group_by(category_name) %>%
  summarise(total_monto = sum(monto, na.rm = TRUE)) %>%
  slice_max(total_monto, n = 12)

ventas_categoria <- order_details %>%
  left_join(products, by = "product_id") %>%
  left_join(categories, by = "category_id") %>%
  count(category_name, name = "total_orders") %>%
  slice_max(total_orders, n = 12)

# Top empleados por monto
ventas_empleado_monto <- order_details %>%
  left_join(orders, by = "order_id") %>%
  left_join(employees, by = "employee_id") %>%
  mutate(employee_name = paste(first_name, last_name)) %>%
  group_by(employee_name) %>%
  summarise(total_monto = sum(monto, na.rm = TRUE)) %>%
  slice_max(total_monto, n = 12)

# Top productos por monto
ventas_producto_monto <- order_details %>%
  left_join(products, by = "product_id") %>%
  group_by(product_name) %>%
  summarise(total_monto = sum(monto, na.rm = TRUE)) %>%
  slice_max(total_monto, n = 12)

# UI
ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Northwind Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Resumen", tabName = "resumen", icon = icon("chart-line")),
      menuItem("Clientes", tabName = "clientes", icon = icon("users")),
      menuItem("Ventas", tabName = "ventas", icon = icon("dollar-sign"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "resumen",
              fluidRow(
                infoBox("Clientes", nrow(customers), icon = icon("user"), color = "blue", width = 4, fill = TRUE),
                infoBox("Órdenes", nrow(orders), icon = icon("receipt"), color = "green", width = 4, fill = TRUE),
                infoBox("Productos", nrow(products), icon = icon("box"), color = "purple", width = 4, fill = TRUE),
                infoBox("Empleados", nrow(employees), icon = icon("building"), color = "red", width = 4, fill = TRUE),
                infoBox("Categorías", nrow(categories), icon = icon("list-ul"), color = "yellow", width = 4, fill = TRUE),
                infoBox("Territorios", nrow(territories), icon = icon("map"), color = "teal", width = 4, fill = TRUE)
              ),
              
              # Ventas por País - horizontal
              fluidRow(
                box(title = "Ventas por País", width = 12,
                    fluidRow(
                      column(width = 6,
                             plotOutput("grafPaisOrdenes")
                      ),
                      column(width = 6,
                             dataTableOutput("tablaPaisMonto")
                      )
                    )
                ),
                # Ventas por Categoría - horizontal
                box(title = "Ventas por Categoría", width = 12,
                    fluidRow(
                      column(width = 6,
                             plotOutput("grafCategoriaOrdenes")
                      ),
                      column(width = 6,
                             dataTableOutput("tablaCategoriaMonto")
                      )
                    )
                )
              ),
              
              # Evolución mensual
              fluidRow(
                box(title = "Evolución mensual de ventas", width = 12,
                    selectInput("anioSeleccionado", "Selecciona el Año:",
                                choices = sort(unique(format(as.Date(orders$order_date), "%Y")))),
                    plotOutput("grafVentasMensual"))
              ),
              
              # Top empleados y productos
              fluidRow(
                box(title = "Top 12 Empleados por Monto", width = 6,
                    plotOutput("grafEmpleadosMonto")),
                box(title = "Top 12 Productos por Monto", width = 6,
                    plotOutput("grafProductosMonto"))
              )
      ),
      
      tabItem(tabName = "clientes",
              box(title = "Listado de Clientes", width = 12,
                  dataTableOutput("tablaClientes"))
      ),
      
      tabItem(tabName = "ventas",
              box(title = "Órdenes", width = 12,
                  dataTableOutput("tablaOrders"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Gráficos
  output$grafPaisOrdenes <- renderPlot({
    ggplot(ventas_pais, aes(x = fct_reorder(country, total_orders), y = total_orders, fill = total_orders)) +
      geom_col() +
      geom_text(aes(label = total_orders), hjust = -0.1) +
      scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
      coord_flip() +
      labs(x = "País", y = "Número de Órdenes", title = "Top 12 Países por Número de Órdenes") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", hjust = 0.5))
  })
  
  output$grafCategoriaOrdenes <- renderPlot({
    ggplot(ventas_categoria, aes(x = fct_reorder(category_name, total_orders), y = total_orders, fill = total_orders)) +
      geom_col() +
      geom_text(aes(label = total_orders), hjust = -0.1) +
      scale_fill_gradient(low = "#A1D490", high = "#0B5345") +
      coord_flip() +
      labs(x = "Categoría", y = "Número de Órdenes", title = "Top 12 Categorías por Número de Órdenes") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", hjust = 0.5))
  })
  
  output$grafVentasMensual <- renderPlot({
    req(input$anioSeleccionado)
    df <- order_details %>%
      left_join(orders, by = "order_id") %>%
      filter(format(as.Date(order_date), "%Y") == input$anioSeleccionado) %>%
      mutate(mes = format(as.Date(order_date), "%m")) %>%
      group_by(mes) %>%
      summarise(total_monto = sum(monto, na.rm = TRUE))
    
    ggplot(df, aes(x = mes, y = total_monto, group = 1)) +
      geom_line(color = "#FF5733", size = 1.2) +
      geom_point(color = "#C70039", size = 3) +
      labs(x = "Mes", y = "Monto Total", title = paste("Evolución Mensual de Ventas", input$anioSeleccionado)) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
  })
  
  output$grafEmpleadosMonto <- renderPlot({
    ggplot(ventas_empleado_monto, aes(x = fct_reorder(employee_name, total_monto), y = total_monto, fill = total_monto)) +
      geom_col() +
      geom_text(aes(label = round(total_monto,0)), hjust = -0.1) +
      scale_fill_gradient(low = "#FFB347", high = "#FF5E62") +
      coord_flip() +
      labs(x = "Empleado", y = "Monto Total", title = "Top 12 Empleados por Monto") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", hjust = 0.5))
  })
  
  output$grafProductosMonto <- renderPlot({
    ggplot(ventas_producto_monto, aes(x = fct_reorder(product_name, total_monto), y = total_monto, fill = total_monto)) +
      geom_col() +
      geom_text(aes(label = round(total_monto,0)), hjust = -0.1) +
      scale_fill_gradient(low = "#FFD700", high = "#FFA500") +
      coord_flip() +
      labs(x = "Producto", y = "Monto Total", title = "Top 12 Productos por Monto") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", hjust = 0.5))
  })
  
  # Tablas
  output$tablaPaisMonto <- renderDataTable({
    df <- ventas_pais_monto %>%
      rename(
        "País" = country,
        "Monto Total" = total_monto
      ) %>%
      arrange(desc(`Monto Total`))
    datatable(df, options = list(pageLength = 10))
  })
  
  output$tablaCategoriaMonto <- renderDataTable({
    df <- ventas_categoria_monto %>%
      rename(
        "Categoría" = category_name,
        "Monto Total" = total_monto
      ) %>%
      arrange(desc(`Monto Total`))
    datatable(df, options = list(pageLength = 10))
  })
  
  output$tablaVentasFiltradas <- renderDataTable({
    df <- order_details %>%
      left_join(orders, by = "order_id") %>%
      filter(format(as.Date(order_date), "%Y") == input$anioSeleccionado) %>%
      select(order_id, customer_id, employee_id, order_date, monto) %>%
      rename(
        "ID Orden" = order_id,
        "Cliente" = customer_id,
        "Empleado" = employee_id,
        "Fecha Orden" = order_date,
        "Monto Total" = monto
      )
    datatable(df)
  })
  
  output$tablaClientes <- renderDataTable({
    df <- customers %>%
      select(customer_id, company_name, contact_name, country, city, phone) %>%
      rename(
        "ID Cliente" = customer_id,
        "Empresa" = company_name,
        "Contacto" = contact_name,
        "País" = country,
        "Ciudad" = city,
        "Teléfono" = phone
      )
    datatable(df)
  })
  
  output$tablaOrders <- renderDataTable({
    df <- orders %>%
      select(order_id, customer_id, order_date, required_date, ship_country, employee_id, ship_name) %>%
      rename(
        "ID Orden" = order_id,
        "Cliente" = customer_id,
        "Fecha Orden" = order_date,
        "Fecha Requerida" = required_date,
        "País Envío" = ship_country,
        "Empleado" = employee_id,
        "Empresa Remitente" = ship_name
      )
    datatable(df)
  })
}

shinyApp(ui, server)

