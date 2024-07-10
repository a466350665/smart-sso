package openjoe.smart.sso.server.controller;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

import java.io.UnsupportedEncodingException;

/**
 * 首页管理
 *
 * @author Joe
 */
@Controller
@RequestMapping("/")
public class IndexController {

    @Value("${server.port}")
    private Integer serverPort;

    /**
     * 初始页
     *
     * @param model
     * @return
     * @throws UnsupportedEncodingException
     */
    @GetMapping
    public String execute(Model model) throws UnsupportedEncodingException {
        // 当前服务端口号
        model.addAttribute("serverPort", serverPort);
        return "index";
    }
}