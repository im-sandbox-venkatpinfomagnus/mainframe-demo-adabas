package com.mainframe.banking.config;

import org.springframework.context.annotation.Configuration;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;

/**
 * JPA Configuration
 * 
 * Enables JPA auditing for automatic timestamp management
 * - @CreatedDate: Set on entity creation
 * - @LastModifiedDate: Updated on every modification
 */
@Configuration
@EnableJpaAuditing
public class JpaConfig {
}
