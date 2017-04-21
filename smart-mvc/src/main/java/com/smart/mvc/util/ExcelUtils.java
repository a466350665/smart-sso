package com.smart.mvc.util;

import java.beans.BeanInfo;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.util.StringUtils;

/**
 * Excel工具类
 * 
 * @author Joe
 */
public class ExcelUtils {
	/**
	 * Excel2003和2007版的最大行数
	 */
	private static final int MAX_ROW_2003 = 65535;
	private static final int MAX_ROW_2007 = 1048575;
	
	/**
	 * 从Excel中导入内容到List集合
	 * 
	 * @param file 待处理的Excel文件
	 * @param type 对应转换的bean类名
	 * @param fieldNames 需要转换为bean的属性名称集合，属性的顺序必须和列的顺序保持一致
	 * @return
	 * @throws IOException 
	 */
	public static <T> List<T> importExcel(File file, Class<T> type,
			String[] fieldNames) throws IOException {
		return importExcel(file, type, fieldNames, 0, 0, 0);
	}

	/**
	 * 从Excel中导入内容到List集合
	 * 
	 * @param file 待处理的Excel文件
	 * @param type 对应转换的bean类名
	 * @param fieldNames 需要转换为bean的属性名称集合，属性的顺序必须和列的顺序保持一致
	 * @param beginRow 需要导入的开始行，小于等于-1都表示从Excel第一行开始导入
	 * @param beginColumn 需要导入的开始列，小于等于-1表示Excel第一列开始
	 * @param sheetIndex 工作表下标
	 * @return
	 * @throws IOException 
	 */
	public static <T> List<T> importExcel(File file, Class<T> type,
			String[] fieldNames, int beginRow, int beginColumn, int sheetIndex)
			throws IOException {
		return importExcel(file, type, fieldNames, beginRow, -1, beginColumn,
				-1, sheetIndex);
	}
	
	/**
	 * 从Excel中导入内容到List集合
	 * 
	 * @param file 待处理的Excel文件
	 * @param type 对应转换的bean类名
	 * @param fieldNames 需要转换为bean的属性名称集合，属性的顺序必须和列的顺序保持一致
	 * @param beginRow 需要导入的开始行，小于等于-1都表示从Excel第一行开始导入
	 * @param endRow 需要导入的结束行，小于等于-1都表示Excel最后一行结束导入，如果超过Excel最后一行，则以Excel的最后一行为结束
	 * @param beginColumn 需要导入的开始列，小于等于-1表示Excel第一列开始
	 * @param endColumn 需要导入的结束列，小于等于-1表示Excel最后一列结束，如果超过Excel最后一列，则以Excel的最后一列为结束
	 * @param sheetIndex 工作表下标
	 * @return
	 * @throws IOException 
	 */
	private static <T> List<T> importExcel(File file, Class<T> type,
			String[] fieldNames, int beginRow, int endRow, int beginColumn,
			int endColumn, int sheetIndex) throws IOException {
		// 创建文件输入流
		FileInputStream in = new FileInputStream(file);
		// 创建Excel工作簿（包括2003和2007版）
		Workbook workbook = createWorkbook(
				FileUtils.getFilenameExtension(file.getName()), in);
		// 根据下标获取Excel工作表
		Sheet sheet = workbook.getSheetAt(sheetIndex);

		return importExcel(in, type, fieldNames, beginRow, endRow, beginColumn,
				endColumn, sheet);
	}
	
	/**
	 * 创建Excel工作簿
	 * 
	 * @param fileName Excel文件名
	 * @param in 文件输入流
	 * @return
	 * @throws IOException
	 */
	private static Workbook createWorkbook(String fileExtension, InputStream in)
			throws IOException {
		Workbook workbook = null;
		if (fileExtension.equals("xlsx")) { // 2007版excel
			if (in != null) {
				workbook = new XSSFWorkbook(in);
			} else {
				workbook = new XSSFWorkbook();
			}
		} else { // 2003版excel
			if (in != null) {
				workbook = new HSSFWorkbook(in);
			} else {
				workbook = new HSSFWorkbook();
			}
		}
		return workbook;
	}
	
	/**
	 * 从Excel中导入内容到List集合
	 * 
	 * @param in 对应文件输入流
	 * @param type 对应转换的bean类名
	 * @param fieldNames 需要转换为bean的属性名称集合，属性的顺序必须和列的顺序保持一致
	 * @param beginRow 需要导入的开始行，小于等于-1都表示从Excel第一行开始导入
	 * @param endRow 需要导入的结束行，小于等于-1都表示Excel最后一行结束导入，如果超过Excel最后一行，则以Excel的最后一行为结束
	 * @param beginColumn 需要导入的开始列，小于等于-1表示Excel第一列开始
	 * @param endColumn 需要导入的结束列，小于等于-1表示Excel最后一列结束，如果超过Excel最后一列，则以Excel的最后一列为结束
	 * @param sheet 工作表
	 * @return
	 */
	private static <T> List<T> importExcel(FileInputStream in, Class<T> type,
			String[] fieldNames, int beginRow, int endRow, int beginColumn,
			int endColumn, Sheet sheet) {
		// 验证起始行是否符合标准
		if (beginRow < 0) {
			beginRow = sheet.getFirstRowNum();
		}
		if (endRow < 0) {
			endRow = sheet.getLastRowNum();
		}
		if (endRow - beginRow < 0) {
			throw new IllegalArgumentException(
					"Start row is more than end row!");
		}

		List<T> list = new ArrayList<T>();
		Row row;
		T t;
		for (int i = beginRow; i <= endRow; i++) {
			// 获取工作表中的某一行
			row = sheet.getRow(i);
			if (row == null)
				continue;

			// 验证起始列是否符合标准
			if (beginColumn < 0) {
				beginColumn = row.getFirstCellNum();
			} else if (beginColumn > row.getLastCellNum()) {
				throw new IllegalArgumentException(
						"Start column is more than last column!");
			}

			if (endColumn < 0) {
				endColumn = row.getLastCellNum();
			} else if (endColumn > row.getLastCellNum()) {
				endColumn = row.getLastCellNum();
			}
			if (endColumn - beginColumn < 0) {
				throw new IllegalArgumentException(
						"Start column is more than end column!");
			}

			// 通过反射创建class的实例
			try {
				t = type.newInstance();
			} catch (Exception e) {
				throw new IllegalArgumentException(
						"The bean class object cannot be instantiated!", e);
			}

			// 解析excel行列数据对应到创建的Bean中
			for (int j = beginColumn, index = 0; j < endColumn
					&& index < fieldNames.length; j++, index++) {
				Cell cell = row.getCell(j);
				if (cell == null)
					continue;
				String propertyName = fieldNames[index];
				try {
					Object value = getCellValue(cell,
							getPropertyType(t, propertyName));
					setPropertyValue(t, propertyName, value);
				} catch (Exception e) {
					throw new IllegalArgumentException(type.getName() + "."
							+ propertyName + " cannot be accessed!", e);
				}
			}
			list.add(t);
		}
		return list;
	}

	/**
	 * 获取excel单元格中的值
	 * 
	 * @param cell excel单元格
	 * @param type 属性的返回类型Class
	 * @return
	 * @throws Exception
	 */
	private static Object getCellValue(Cell cell, Class<?> type)
			throws Exception {
		Object value = null;
		try {
			if (type == Date.class) {
				value = cell.getDateCellValue();
			} else {
				value = getCellValue(cell, cell.getCellType());
				if (value == null) {
					return null;
				}
				if (type == int.class) {
					if (value instanceof String) {
						if (!StringUtils.isEmpty((String) value)) {
							value = Integer.valueOf((String) value);
						} else {
							value = 0;
						}
					} else if (value instanceof Double) {
						value = ((Double) value).intValue();
					}
				} else if (type == double.class) {
					if (value instanceof String) {
						if (!StringUtils.isEmpty((String) value)) {
							value = Double.valueOf((String) value);
						} else {
							value = 0;
						}
					} else if (value instanceof Integer) {
						value = Double.valueOf((Integer) value);
					}
				} else if (type == boolean.class) {
					if (value.getClass() == double.class
							|| value.getClass() == int.class) {
						if ((Integer) value == 1) {
							value = true;
						} else {
							value = false;
						}
					} else if (value instanceof String) {
						value = ((String) value).trim();
						if ("1".equals(value)
								|| "y".equalsIgnoreCase((String) value)
								|| "yes".equalsIgnoreCase((String) value)
								|| "true".equalsIgnoreCase((String) value)) {
							value = true;
						} else {
							value = false;
						}
					}
				}
			}
			return value;
		} catch (Exception e) {
			return null;
		}
	}

	/**
	 * 获取excel单元格中的值
	 * 
	 * @param cell excel单元格
	 * @param cellType excel单元格类型
	 * @return
	 */
	private static Object getCellValue(Cell cell, int cellType) {
		Object value = null;
		if (cellType == Cell.CELL_TYPE_BLANK) {
			return null;
		} else if (cellType == Cell.CELL_TYPE_FORMULA) {
			value = getCellValue(cell, cell.getCachedFormulaResultType());
		} else if (cellType == Cell.CELL_TYPE_NUMERIC) {
			cell.setCellType(Cell.CELL_TYPE_STRING);
			String temp = cell.getStringCellValue();
			// 判断是否包含小数点，如果不含小数点，则以字符串读取，如果含小数点，则转换为Double类型的字符串
			if (temp.indexOf(".") > -1) {
				temp = String.valueOf(new Double(temp)).trim();
			} else {
				temp = temp.trim();
			}
			value = temp;
		} else if (cellType == Cell.CELL_TYPE_BOOLEAN) {
			value = cell.getBooleanCellValue();
		} else if (cellType == Cell.CELL_TYPE_STRING) {
			value = cell.getRichStringCellValue().getString();
		}
		return value;
	}

	/**
	 * 导出内容到Excel文件
	 * 
	 * @param fileName 可包含文件路径的Excel文件名称
	 * @param collection 导出到Excel的内容集合
	 * @return
	 * @throws IOException
	 */
	public static File exportExcel(String fileName, Collection<?> collection)
			throws IOException {
		return exportExcel(fileName, collection, null, null);
	}
	
	/**
	 * 导出内容到Excel文件
	 * 
	 * @param fileName 可包含文件路径的Excel文件名称
	 * @param columnHeaders Excel文件列头
	 * @param fieldNames Bean的属性名称集合，属性的顺序必须和列的顺序保持一致
	 * @param dataMap 导出到Excel的多个sheet集合的Map
	 * @return
	 * @throws IOException
	 */
	public static File exportExcel(String fileName, String[] columnHeaders,
			String[] fieldNames, Map<String, Collection<?>> dataMap)
			throws IOException {
		if (dataMap != null) {
			Workbook workbook = null;
			String fileExtension = FileUtils.getFilenameExtension(fileName);
			for (String key : dataMap.keySet()) {
				workbook = createWorkbook(workbook, fileExtension,
						dataMap.get(key), columnHeaders, fieldNames, key);
			}
			return writeFile(fileName, workbook);
		}
		return null;
	}

	/**
	 * 导出内容到Excel文件
	 * 
	 * @param fileName 可包含文件路径的Excel文件名称
	 * @param collection 导出到Excel的内容集合
	 * @param columnHeaders Excel文件列头
	 * @param fieldNames Bean的属性名称集合，属性的顺序必须和列的顺序保持一致
	 * @return
	 * @throws IOException
	 */
	public static File exportExcel(String fileName, Collection<?> collection,
			String[] columnHeaders, String[] fieldNames) throws IOException {
		File file = null;
		String fileExtension = FileUtils.getFilenameExtension(fileName);
		int maxRow = MAX_ROW_2003;
		if ("xlsx".equals(fileExtension)) {
			maxRow = MAX_ROW_2007;
		}
		if (collection.size() > maxRow) {
			file = writeFile(
					fileName,
					loopWorkbook(null, fileExtension, new ArrayList<Object>(
							collection), columnHeaders, fieldNames, 0, maxRow,
							maxRow, 1));
		} else {
			file = writeFile(
					fileName,
					createWorkbook(null, fileExtension, collection,
							columnHeaders, fieldNames, "sheet1"));
		}
		return file;
	}
	
	/**
	 * 循环的创建Excel工作簿
	 * (当数据集合大于excel工作表的最大行数时，创建多个工作表填充数据,sheet1、sheet2、sheet3...)
	 * 
	 * @param workbook excel工作簿
	 * @param fileExtension 文件扩展名
	 * @param columnHeaders Excel文件列头
	 * @param fieldNames Bean的属性名称集合，属性的顺序必须和列的顺序保持一致
	 * @param list 导出到Excel的内容集合
	 * @param fromIndex list集合开始下标（包含本身）
	 * @param toIndex list集合结束下标（不包含本身）
	 * @param maxRow excel可支持的最大行数
	 * @param sheetIndex 工作表下标
	 * @return
	 * @throws IOException
	 */
	private static Workbook loopWorkbook(Workbook workbook,
			String fileExtension, List<Object> list, String[] columnHeaders,
			String[] fieldNames, int fromIndex, int toIndex, int maxRow,
			int sheetIndex) throws IOException {
		if (toIndex == list.size()) {
			return createWorkbook(workbook, fileExtension,
					list.subList(fromIndex, toIndex), columnHeaders,
					fieldNames, "sheet" + sheetIndex);

		} else {
			workbook = createWorkbook(workbook, fileExtension,
					list.subList(fromIndex, toIndex), columnHeaders,
					fieldNames, "sheet" + sheetIndex);
			return loopWorkbook(workbook, fileExtension, list, columnHeaders,
					fieldNames, toIndex,
					(toIndex + maxRow) > list.size() ? list.size()
							: (toIndex + maxRow), maxRow, ++sheetIndex);
		}
	}

	/**
	 * 导出内容到Excel文件
	 * 
	 * @param fileName 可包含文件路径的Excel文件名称
	 * @param collection 导出到Excel的内容集合
	 * @param columnHeaders Excel文件列头
	 * @param fieldNames Bean的属性名称集合，属性的顺序必须和列的顺序保持一致
	 * @param sheetName 工作表名称
	 * @return
	 * @throws IOException
	 */
	private static Workbook createWorkbook(Workbook workbook,
			String fileExtension, Collection<?> collection,
			String[] columnHeaders, String[] fieldNames, String sheetName)
			throws IOException {
		if (workbook == null) {
			workbook = createWorkbook(fileExtension, null);
		}
		Sheet sheet = workbook.createSheet(sheetName);

		// 产生表格标题行
		int rowIndex = sheet.getLastRowNum();
		if (columnHeaders != null && columnHeaders.length > 0) {
			Row row = sheet.createRow(rowIndex);
			for (int i = 0; i < columnHeaders.length; i++) {
				Cell cell = row.createCell(i);
				cell.setCellValue(columnHeaders[i]);
			}
			rowIndex++;
		}

		// 产生Excel表数据
		Iterator<?> ites = collection.iterator();
		// 属性类型
		Class<?> propertyType;
		// 属性值
		Object value;
		// 日期格式
		CellStyle dateCellStyle = null;
		while (ites.hasNext()) {
			Row row = sheet.createRow(rowIndex++);
			Object t = ites.next();

			if (fieldNames == null) {
				fieldNames = getFieldNames(t);
			}

			for (int columnIndex = 0; columnIndex < fieldNames.length; columnIndex++) {
				try {
					propertyType = getPropertyType(t, fieldNames[columnIndex]);
					value = getPropertyValue(t, fieldNames[columnIndex]);
				} catch (Exception e) {
					throw new IllegalArgumentException(t.getClass().getName()
							+ "." + fieldNames[columnIndex]
							+ " cannot be accessed!", e);
				}
				if (value == null)
					continue;

				Cell cell = row.createCell(columnIndex);
				if (propertyType == Boolean.class) {
					cell.setCellValue((Boolean) value ? "true" : "false");
				} else if (propertyType == Date.class) {
					if (dateCellStyle == null) {
						dateCellStyle = createCellStyle(workbook);
					}
					cell.setCellValue((Date) value);
					cell.setCellStyle(dateCellStyle);
				} else if (propertyType == double.class) {
					cell.setCellValue(new DecimalFormat("0.00#").format((Double) value));
				} else if (propertyType == int.class) {
					cell.setCellValue((Integer) value);
				} else {
					cell.setCellValue(value.toString());
				}
			}
		}
		return workbook;
	}
	
	/**
	 * 把创建的excel工作簿写入到物理路径
	 * 
	 * @param fileName 包含路径的文件名称
	 * @param workbook excel工作簿
	 * @return
	 * @throws IOException
	 */
	private static File writeFile(String fileName, Workbook workbook)
			throws IOException {
		File file = FileUtils.createFile(fileName);
		FileOutputStream out = new FileOutputStream(file);
		workbook.write(out);
		out.close();
		return file;
	}

	/**
	 * 为日期类型创建工作表中单元格样式
	 * 
	 * @param workbook 工作簿
	 * @return
	 */
	private static CellStyle createCellStyle(Workbook workbook) {
		CellStyle dateCellStyle = workbook.createCellStyle();
		short df = workbook.createDataFormat().getFormat("yyyy-MM-dd");
		dateCellStyle.setDataFormat(df);
		return dateCellStyle;
	}

	/**
	 * 通过反射处理，设置某个对象中的某个属性值
	 * 
	 * @param obj 指定对象
	 * @param propertyName 对象属性名称
	 * @param value 对象属性值
	 */
	private static void setPropertyValue(Object obj, String propertyName,
			Object value) {
		if (obj == null) {
			return;
		}
		try {
			BeanInfo beanInfo = Introspector.getBeanInfo(obj.getClass());
			PropertyDescriptor[] propertyDescriptors = beanInfo
					.getPropertyDescriptors();
			for (PropertyDescriptor property : propertyDescriptors) {
				if (property.getName().equals(propertyName)) {
					Method setter = property.getWriteMethod();
					setter.invoke(obj, value);
				}
			}
		} catch (Exception e) {
			throw new IllegalArgumentException(e);
		}
	}

	/**
	 * 通过反射处理，获取某个对象中的某个属性值
	 * 
	 * @param obj 指定对象
	 * @param propertyName 对象属性名称
	 * @return
	 */
	private static Object getPropertyValue(Object obj, String propertyName) {
		if (obj == null) {
			return null;
		}
		if(obj instanceof Map){
			@SuppressWarnings("rawtypes")
			Map m=(Map) obj;
			return m.get(propertyName);
		}else{
			try {
				BeanInfo beanInfo = Introspector.getBeanInfo(obj.getClass());
				PropertyDescriptor[] propertyDescriptors = beanInfo
				.getPropertyDescriptors();
				for (PropertyDescriptor property : propertyDescriptors) {
					if (property.getName().equals(propertyName)) {
						Method getter = property.getReadMethod();
						return getter.invoke(obj);
					}
				}
			} catch (Exception e) {
				throw new IllegalArgumentException(e);
			}
		}
		
		return null;
	}

	/**
	 * 通过反射处理，获取某个对象中的某个属性的返回类型Class
	 * 
	 * @param obj 指定对象
	 * @param propertyName 对象属性名称
	 * @return
	 */
	private static Class<?> getPropertyType(Object obj, String propertyName) {
		if (obj == null) {
			return null;
		}
		try {
			BeanInfo beanInfo = Introspector.getBeanInfo(obj.getClass());
			PropertyDescriptor[] propertyDescriptors = beanInfo
					.getPropertyDescriptors();
			for (PropertyDescriptor property : propertyDescriptors) {
				if (property.getName().equals(propertyName)) {
					return property.getPropertyType();
				}
			}
		} catch (Exception e) {
			throw new IllegalArgumentException(e);
		}
		return null;
	}

	/**
	 * 通过反射处理，获取某个对象中的所有属性名称，并添加到字符串数组返回
	 * 
	 * @param obj 指定对象
	 * @return
	 */
	private static String[] getFieldNames(Object obj) {
		if (obj == null) {
			return null;
		}
		String[] fieldNames = null;
		try {
			BeanInfo beanInfo = Introspector.getBeanInfo(obj.getClass());
			PropertyDescriptor[] propertyDescriptors = beanInfo
					.getPropertyDescriptors();
			fieldNames = new String[propertyDescriptors.length - 1];
			int i = 0;
			for (PropertyDescriptor property : propertyDescriptors) {
				String key = property.getName();
				// 过滤class属性
				if (!key.equals("class")) {
					fieldNames[i++] = key;
				}
			}
		} catch (Exception e) {
			throw new IllegalArgumentException(e);
		}
		return fieldNames;
	}
}
