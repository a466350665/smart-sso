package com.smart.mvc.util;


import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * 使用Junit 4 进行单元白盒测试 demo
 *
 * @author bluetata / Sekito.Lv@gmail.com</br>
 * @version smart version(1.0)</br>
 * @date 09/05/18 17:05</br>
 * @since JDK 1.8</br>
 */
public class StringUtilsTest {

    private StringUtils stringUtils;
    @Before
    public void init() {
        stringUtils = new StringUtils();
    }

    @Test
    public void isBlank() {
        assertEquals(true,stringUtils.isBlank(" "));
    }

    @After
    public void tearDown() {
        stringUtils = null;
    }
}
