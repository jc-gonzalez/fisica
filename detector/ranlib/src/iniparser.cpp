/******************************************************************************
 * File:    ranlib.cpp
 *
 * Domain:  Config.Config.ranlib
 *
 * Version: 1.0
 *
 * Date:    2015/07/01
 *
 * Copyright (C) 2015 J C Gonzalez
 *_____________________________________________________________________________
 *
 * Topic: General Information
 *
 * Purpose:
 *   Implement Ranlib class
 *
 * Created by:
 *   J C Gonzalez
 *
 * Status:
 *   Prototype
 *
 * Dependencies:
 *   none
 *
 * Files read / modified:
 *   none
 *
 * History:
 *   See <Changelog>
 *
 * About: License Conditions
 *   See <License>
 *
 ******************************************************************************/

#include "ranlib.h"

#include <iostream>
#include <fstream>
#include <sstream>

////////////////////////////////////////////////////////////////////////////
// Namespace: Config
// -----------------------
//
// Library namespace
////////////////////////////////////////////////////////////////////////////
namespace Config {

template<class T>
inline std::string toStr(const T & x)
{
  std::stringstream ss;
  ss << x;
  return ss.str();
}

template<class T>
inline T strTo(const std::string & s)
{
  std::stringstream ss(s);
  T x;
  ss >> x;
  return x;
}

//----------------------------------------------------------------------
// Method: createConfig
//----------------------------------------------------------------------
Config * Ranlib::createConfig()
{
  config = new Config;
  return config;
}

//----------------------------------------------------------------------
// Method: createConfig
//----------------------------------------------------------------------
void Ranlib::destroyConfig(Config * c)
{
  if (c != 0) { delete c; }
}

//----------------------------------------------------------------------
// Method: with
//----------------------------------------------------------------------
Ranlib & Ranlib::with(Config * cfg)
{
  config = cfg;
  lastSection = "General";
  return *this;
}

//----------------------------------------------------------------------
// Method: section
//----------------------------------------------------------------------
Ranlib & Ranlib::section(std::string section)
{
  lastSection = section;
  return *this;
}

//----------------------------------------------------------------------
// Method: add
//----------------------------------------------------------------------
Ranlib & Ranlib::add(std::string key, std::string value)
{
  return add(lastSection, key, value);
}

//----------------------------------------------------------------------
// Method: add
//----------------------------------------------------------------------
Ranlib & Ranlib::add(std::string section, std::string key, std::string value)
{
  (*config)[section].insert(std::pair<std::string, std::string>(key, value));
  lastSection = section;
  return *this;
}

//----------------------------------------------------------------------
// Method: add
//----------------------------------------------------------------------
template<class T>
Ranlib & Ranlib::add(std::string key, T value)
{
  return add<T>(lastSection, key, value);
}

//----------------------------------------------------------------------
// Method: add
//----------------------------------------------------------------------
template<class T>
Ranlib & Ranlib::add(std::string section, std::string key, T value)
{
  std::string strValue = toStr<T>(value);
  return add(section, key, strValue);
}

//----------------------------------------------------------------------
// Method: remove
//----------------------------------------------------------------------
Ranlib & Ranlib::remove(std::string key)
{
  return remove(lastSection, key);
}

//----------------------------------------------------------------------
// Method: remove
//----------------------------------------------------------------------
Ranlib & Ranlib::remove(std::string section, std::string key)
{
  (*config)[section].erase(key);
  lastSection = section;
  return *this;
}

//----------------------------------------------------------------------
// Method: exists
//----------------------------------------------------------------------
bool Ranlib::exists(std::string key)
{
  return exists(lastSection, key);
}

//----------------------------------------------------------------------
// Method: exists
//----------------------------------------------------------------------
bool Ranlib::exists(std::string section, std::string key)
{
  std::map<std::string, 
           std::map<std::string, std::string> >::iterator it;
  it = config->find(section); 
  if (it != config->end()) {
    if ((*it).second.find(key) != (*it).second.end()) {
      return true;
    }
  }
  return false;
}

//----------------------------------------------------------------------
// Method: valueOf
//----------------------------------------------------------------------
std::string Ranlib::valueOf(std::string key)
{
  return valueOf(lastSection, key);
}

//----------------------------------------------------------------------
// Method: valueOf
//----------------------------------------------------------------------
std::string Ranlib::valueOf(std::string section, std::string key)
{
  lastSection = section;
  return (*config)[section][key];
}

//----------------------------------------------------------------------
// Method: getListOf
//----------------------------------------------------------------------
Ranlib & Ranlib::getListOf(std::string key, std::vector<std::string> & vec)
{
  std::string v = valueOf(lastSection, key);
  std::istringstream ss(v);
  std::string elem;
  while (std::getline(ss, elem, ',')) {
    vec.push_back(elem);
  }  
  return *this;
}

//----------------------------------------------------------------------
// Method: getVectorOf
//----------------------------------------------------------------------
Ranlib & Ranlib::getVectorOf(std::string key, std::vector<int> & vec)
{
  std::string v = valueOf(lastSection, key);
  std::istringstream ss(v);
  std::string elem;
  while (std::getline(ss, elem, ',')) {
    vec.push_back(strTo<int>(elem));
  }  
  return *this;
}

//----------------------------------------------------------------------
// Method: getKeys
//----------------------------------------------------------------------
Ranlib & Ranlib::getKeys(std::string section, std::vector<std::string> & vec)
{
  std::map<std::string, std::string>::iterator it =  ((*config)[section]).begin();
  vec.clear();
  while (it != ((*config)[section]).end()) {
    vec.push_back(it->first);
    ++it;
  }  
  return *this;
}

//----------------------------------------------------------------------
// Method: valueAs
//----------------------------------------------------------------------
template<class T>
T Ranlib::valueAs(std::string key)
{
  return valueAs<T>(lastSection, key);
}

//----------------------------------------------------------------------
// Method: valueAs
//----------------------------------------------------------------------
template<class T>
T Ranlib::valueAs(std::string section, std::string key)
{
  lastSection = section;
  std::string value = ((*config)[section])[key];
  T x = strTo<T>(value);
  return  x;
}

//----------------------------------------------------------------------
// Method: saveTo
//----------------------------------------------------------------------
bool Ranlib::saveTo(Config * config, const char * fileName)
{
  std::fstream outFs(fileName, std::ios::out | std::ios::trunc);
  std::map<std::string,
	   std::map<std::string, std::string> >::iterator it = config->begin();
  while (it != config->end()) {
    std::string sec = it->first;

    outFs << ';' << std::string(75, '-') << std::endl;
    outFs << "; Section: " << sec << std::endl;
    outFs << ';' << std::string(75, '-') << std::endl;
    outFs << '[' << sec << ']' << std::endl;
    KeyValueMap & pairs = it->second;
    std::map<std::string, std::string>::iterator itPairs = pairs.begin();
    while (itPairs != pairs.end()) {
      outFs << itPairs->first << '=' << itPairs->second << std::endl;
      ++itPairs;
    }
    outFs << std::endl;
    ++it;
  }
  outFs.close();

  return true;
}

//----------------------------------------------------------------------
// Method: loadFrom
//----------------------------------------------------------------------
bool Ranlib::loadFrom(Config * config, const char * fileName)
{
  bool isSuccessful = true;
  std::fstream inFs(fileName, std::ios::in | std::ios::binary);

  if (inFs) {

    inFs.seekg(0, inFs.end);
    int length = inFs.tellg();
    inFs.seekg(0, inFs.beg);
    char * buffer = new char [length + 1];
    inFs.read(buffer, length);
    buffer[length] = 0;
    inFs.close();

    isSuccessful = parse(config, buffer);

  } else {
    isSuccessful = false;
  }

  return isSuccessful;
}

//----------------------------------------------------------------------
// Method: parse
//----------------------------------------------------------------------
bool Ranlib::parse(Config * config, const char * content)
{
  bool isSuccessful = true;

  with(config);
  std::istringstream cfg(content);
  std::string section = "General";
  std::string line;
  while (std::getline(cfg, line)) {
    if (line.length() < 1) continue;
    std::istringstream isLine(line);
    if ((line.at(0) == '#') || (line.at(0) == ';')) {
      // Ignore line starting with # or ;
    } else if (line.at(0) == '[') {
      std::getline(isLine, section, '[');
      std::getline(isLine, section, ']');
    } else {      
      std::string key;
      if (std::getline(isLine, key, '=')) {
	std::string value;
	if (std::getline(isLine, value)) {
	  add(section, key, value);
	} else {
	  isSuccessful = false;
	}
      } else {
	isSuccessful = false;
      }
    }
  }

  return isSuccessful;
}

// Template instantiations

template Ranlib & Ranlib::add(std::string, std::string, char const *);
template Ranlib & Ranlib::add(std::string, std::string, char *);
template Ranlib & Ranlib::add(std::string, std::string, char);
template Ranlib & Ranlib::add(std::string, std::string, short);
template Ranlib & Ranlib::add(std::string, std::string, int);
template Ranlib & Ranlib::add(std::string, std::string, long);
template Ranlib & Ranlib::add(std::string, std::string, float);
template Ranlib & Ranlib::add(std::string, std::string, double);

template Ranlib & Ranlib::add(std::string, char const *);
template Ranlib & Ranlib::add(std::string, char *);
template Ranlib & Ranlib::add(std::string, char);
template Ranlib & Ranlib::add(std::string, short);
template Ranlib & Ranlib::add(std::string, int);
template Ranlib & Ranlib::add(std::string, long);
template Ranlib & Ranlib::add(std::string, float);
template Ranlib & Ranlib::add(std::string, double);

template char *       Ranlib::valueAs(std::string);
template char	      Ranlib::valueAs(std::string);
template short        Ranlib::valueAs(std::string);
template int	      Ranlib::valueAs(std::string);
template long         Ranlib::valueAs(std::string);
template float        Ranlib::valueAs(std::string);
template double       Ranlib::valueAs(std::string);

template char *       Ranlib::valueAs(std::string, std::string);
template char	      Ranlib::valueAs(std::string, std::string);
template short        Ranlib::valueAs(std::string, std::string);
template int	      Ranlib::valueAs(std::string, std::string);
template long         Ranlib::valueAs(std::string, std::string);
template float        Ranlib::valueAs(std::string, std::string);
template double       Ranlib::valueAs(std::string, std::string);

}


