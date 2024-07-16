package openjoe.smart.sso.server;

import org.springframework.boot.CommandLineRunner;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.ViewControllerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import javax.servlet.ServletContext;

@Configuration
public class WebMvcConfig implements WebMvcConfigurer {

    @Override
    public void addViewControllers(ViewControllerRegistry registry) {
        registry.addViewController("/").setViewName("redirect:/admin/admin");
    }

    @Override
    public void addResourceHandlers(ResourceHandlerRegistry registry) {
        registry.addResourceHandler("/static/**").addResourceLocations("classpath:/static/");
    }

    @Bean
    public CommandLineRunner commandLineRunner(WebApplicationContext wac) {
        return args -> {
            // 获取ServletContext
            ServletContext servletContext = wac.getServletContext();

            // servletContext设置值
            servletContext.setAttribute("_staticPath", servletContext.getContextPath());
            servletContext.setAttribute("_systemName", "smart-sso单点登录权限系统");
        };
    }
}
