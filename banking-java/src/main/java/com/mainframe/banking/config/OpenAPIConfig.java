package com.mainframe.banking.config;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;
import io.swagger.v3.oas.models.servers.Server;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.List;

/**
 * OpenAPI/Swagger Configuration
 * 
 * Provides interactive API documentation at /swagger-ui.html
 * Replaces mainframe program documentation with modern API docs
 */
@Configuration
public class OpenAPIConfig {
    
    @Bean
    public OpenAPI accountServiceAPI() {
        Server localServer = new Server()
            .url("http://localhost:8080")
            .description("Local development server");
        
        Contact contact = new Contact()
            .name("Mainframe Modernization Team")
            .email("modernization@banking.com");
        
        Info info = new Info()
            .title("Banking Account Service API")
            .version("1.0.0")
            .contact(contact)
            .description("RESTful API modernized from legacy COBOL mainframe applications. " +
                        "Replicates functionality of ACCOUNT-READ.cbl and ACCOUNT-UPDATE.cbl programs.")
            .termsOfService("https://banking.com/terms")
            .license(new License()
                .name("Proprietary")
                .url("https://banking.com/license"));
        
        return new OpenAPI()
            .info(info)
            .servers(List.of(localServer));
    }
}
