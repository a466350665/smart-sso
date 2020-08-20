package com.smart.mvc.model;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;


/**
 * 分页包含列表list属性基类
 * 
 * @author Joe
 * @param <T>
 */
public class Page<T> extends PageSupport implements Serializable {
	
	private static final long serialVersionUID = 7002501955628078021L;
	/** 当前页的数据 */
	private List<T> list = Collections.emptyList();

	private Page() {
	    super();
	}
	
	private Page(int pageNo, int pageSize) {
        super(pageNo, pageSize);
    }
	
    public static <T> Page<T> create() {
        return new Page<>();
    }

    public static <T> Page<T> create(int pageNo, int pageSize) {
        return new Page<>(pageNo, pageSize);
    }

	/**
	 * 获得分页内容
	 * 
	 * @return
	 */
	public List<T> getList() {
		return list;
	}

	/**
	 * 设置分页内容
	 * 
	 * @param list
	 */
	public void setList(List<T> list) {
		this.list = list;
	}
	
	/**
     * 为流程部署页面jsp分页使用
     */
    public String toString() {

        StringBuilder sb = new StringBuilder();
        String funcName = "page";
        String pageSize = getPageSize()+"";
        String funcParam = "";
        int length = 8;// 显示页面长度
        int slider = 1;// 前后显示页面长度
        int first = 1;
        int last = (int)(getRowCount() / (getPageSize() < 1 ? 20 : getPageSize()) + first - 1);
        int pageNo = getPageNo();
        int prev = 0;
        if (getPageNo() > 1) {
            prev = getPageNo() - 1;
        } else {
            prev = first;
        }
        int next = 0;
        if (getPageNo() < last - 1) {
            next = getPageNo() + 1;
        } else {
            next = last;
        }
        String message = "";
        
        if (this.getPageNo() == first) {// 如果是首页
            sb.append("<li class=\"disabled\"><a href=\"javascript:\">&#171; 上一页</a></li>\n");
        } else {
            sb.append("<li><a href=\"javascript:\" onclick=\""+funcName+"("+prev+","+pageSize+",'"+funcParam+"');\">&#171; 上一页</a></li>\n");
        }

        int begin = pageNo - (length / 2);

        if (begin < first) {
            begin = first;
        }

        int end = begin + length - 1;

        if (end >= last) {
            end = last;
            begin = end - length + 1;
            if (begin < first) {
                begin = first;
            }
        }

        if (begin > first) {
            int i = 0;
            for (i = first; i < first + slider && i < begin; i++) {
                sb.append("<li><a href=\"javascript:\" onclick=\""+funcName+"("+i+","+pageSize+",'"+funcParam+"');\">"
                        + (i + 1 - first) + "</a></li>\n");
            }
            if (i < begin) {
                sb.append("<li class=\"disabled\"><a href=\"javascript:\">...</a></li>\n");
            }
        }

        for (int i = begin; i <= end; i++) {
            if (i == pageNo) {
                sb.append("<li class=\"active\"><a href=\"javascript:\">" + (i + 1 - first)
                        + "</a></li>\n");
            } else {
                sb.append("<li><a href=\"javascript:\" onclick=\""+funcName+"("+i+","+pageSize+",'"+funcParam+"');\">"
                        + (i + 1 - first) + "</a></li>\n");
            }
        }

        if (last - end > slider) {
            sb.append("<li class=\"disabled\"><a href=\"javascript:\">...</a></li>\n");
            end = last - slider;
        }

        for (int i = end + 1; i <= last; i++) {
            sb.append("<li><a href=\"javascript:\" onclick=\""+funcName+"("+i+","+pageSize+",'"+funcParam+"');\">"
                    + (i + 1 - first) + "</a></li>\n");
        }

        if (pageNo == last) {
            sb.append("<li class=\"disabled\"><a href=\"javascript:\">下一页 &#187;</a></li>\n");
        } else {
            sb.append("<li><a href=\"javascript:\" onclick=\""+funcName+"("+next+","+pageSize+",'"+funcParam+"');\">"
                    + "下一页 &#187;</a></li>\n");
        }

        sb.append("<li class=\"disabled controls\"><a href=\"javascript:\">当前 ");
        sb.append("<input type=\"text\" value=\""+pageNo+"\" onkeypress=\"var e=window.event||event;var c=e.keyCode||e.which;if(c==13)");
        sb.append(funcName+"(this.value,"+pageSize+",'"+funcParam+"');\" onclick=\"this.select();\"/> / ");
        sb.append("<input type=\"text\" value=\""+pageSize+"\" onkeypress=\"var e=window.event||event;var c=e.keyCode||e.which;if(c==13)");
        sb.append(funcName+"("+pageNo+",this.value,'"+funcParam+"');\" onclick=\"this.select();\"/> 条，");
        sb.append("共 " + getRowCount() + " 条"+(message!=null?message:"")+"</a></li>\n");

        sb.insert(0,"<ul>\n").append("</ul>\n");
        
        sb.append("<div style=\"clear:both;\"></div>");

//      sb.insert(0,"<div class=\"page\">\n").append("</div>\n");
        
        return sb.toString();
    }
}
