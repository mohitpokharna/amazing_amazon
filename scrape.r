library("rvest")
library("stringr")

k=1
s_name=c()
s_url=c()
s_star=c()
s_rat=c()
p_name=c()
s_price=c()
s_shipinfo=c()
s_cod=c()
s_positive=c()
s_amful=c()

fst_url="http://www.amazon.in/gp/bestsellers/kitchen/ref=zg_bs_kitchen_pg_2?ie=UTF8&pg=1"
#first page's url

for(v in 1:5){
  fst_url=substr(fst_url,1,nchar(fst_url)-1)
  fst_url=sprintf("%s%d",fst_url,v)
  
  main_url=read_html(fst_url) %>%
    html_nodes(".zg_title a") %>%
    html_attr("href")
  main_url=gsub("\n","",main_url)
  main_url=gsub(" ","",main_url)
#finds all product url's on the page
  
    
  product=read_html(fst_url) %>%
    html_nodes(".zg_title a") %>%
    html_text()
#finds all product names on the page
    
  for(i in 1:length(main_url)){
    
    url=tryCatch(read_html(main_url[i]) %>%
                   html_node("#olp_feature_div a") %>%
                   html_attr("href"), error=function(e) NA)
    url=paste0("http://www.amazon.in",url)
    url=paste0(url,"&startIndex=00")
    
    url_sellers=tryCatch(read_html(main_url[i]) %>%
                           html_node("#olp_feature_div a") %>%
                           html_text(),error=function(e) NA)
    z=gregexpr('[0-9]+',url_sellers)
    url_sellers=regmatches(url_sellers,z)
    url_sellers=as.numeric(url_sellers)
    if(url!="http://www.amazon.inNA&startIndex=00"){
      x=as.integer((url_sellers-1)/10)+1
      for(y in 1:x){
        seller=c()
        seller_url=c()
        price=c()
        codt=c()
        cod=c()
        shipinfo=c()
        pos=c()
        amful=c()
        z=NA
        
        print(y)
        url=substr(url,1,nchar(url)-2)
        url=sprintf("%s%d0",url,y-1)
        
        while(is.na(z))
        {
          z=read_html(url)
        }
        Sys.sleep(5)
        while(length(seller_url)==0){
          seller_url=read_html(url) %>% html_nodes(".olpSellerColumn .a-spacing-small a") %>% html_attr("href")
        }
        seller_url=paste0("http://www.amazon.in",seller_url)
        
        Sys.sleep(3)
        while(length(seller)==0){
          seller=read_html(url) %>% html_nodes(".olpSellerName") %>% html_text()
        }
        seller=gsub("  ","",seller)
        seller=gsub("\n","",seller)
        #list of all the sellers for the product
        h=1
        s=c()
        for(w in 1:length(seller)){
          if(nchar(seller[w])==0){
            s=read_html(url) %>% html_nodes(".olpSellerName img") %>% html_attr("alt")
            seller[w]=s[h]
            print(s[h])
            h=h+1
          }
        }
        
        while(length(price)==0){
          price=read_html(url) %>% html_nodes(".a-text-bold > span") %>% html_text()
          #price of the product
        }
        while(length(shipinfo)==0){
          shipinfo=read_html(url) %>% html_nodes(".olpShippingInfo") %>% html_text()
        }
        shipinfo=gsub("  ","",shipinfo)
        shipinfo=gsub("\n\n"," ",shipinfo)
        #delivery charges
        
        while(length(codt)==0){
          codt=read_html(url) %>% html_nodes(".a-span2") %>% html_text()
        }
        codt=gsub(price,"",codt)
        codt=gsub(shipinfo,"",codt)
        codt=gsub("Delivery","",codt)
        codt=gsub("Details","",codt)
        j=4
        for(g in 1:10){
          cod[g]=codt[j]
          #whether cash on delivery is available
          j=j+3
        }
        while(length(pos)==0){
          pos=read_html(url) %>% html_nodes(".a-spacing-small b") %>% html_text()
          # % positive reviews
        }
        while(length(amful)==0){
          amful=sub(".+isAmazonFulfilled=(.+)&se.+","\\1",seller_url)
          #whether a fulfilled by amazon item
        }
        print(product[i])
        print(seller)
        
        seller_star=c()
        seller_rat=c()
        Sys.sleep(2)        
        for(j in 1:length(seller_url)){
          a=read_html(seller_url[j]) %>% html_nodes("div.feedbackMeanRating b") %>% html_text()
          seller_star[j] <- a[1]
          #Seller's star rating
          seller_rat[j] <- a[2]
          #Number of ratings
        }
        
        for(j in 1:length(seller_url)){
          p_name[k]=product[i]
          s_name[k]=seller[j]
          s_url[k]=seller_url[j]
          s_star[k]=seller_star[j]
          s_rat[k]=seller_rat[j]
          s_price[k]=price[j]
          s_shipinfo[k]=shipinfo[j]
          s_cod[k]=cod[j]
          s_positive[k]=pos[j]
          s_amful[k]=amful[j]
          k=k+1
        }
      }
    }
  }
}
ref_db=data.frame(Product=p_name,Seller=s_name,Star=s_star,No.ofRatings=s_rat,Price=s_price,Shipping_Info=s_shipinfo,COD=s_cod,Positive=s_positive,Amazon_Fulfilled=s_amful,URL=s_url)

s_id=str_sub(s_url,-14,-1)
s_id=gsub("/www.amazon.in","NA",s_id)
s_id=gsub("=","",s_id)
s_id=gsub("r=","",s_id)
#Seller ID
ref_db=cbind(Seller_ID=s_id,ref_db)
rurl="http://www.amazon.in/gp/aag/ajax/paginatedFeedback.html?seller=A2E4FLA5VMHWJQ&isAmazonFulfilled=1&isCBA=&marketplaceID=A21TJRUUN4KGV&asin=B00IAPD2X4&ref_=aag_m_fb&&currentPage=1"
id=sub(".+seller=(.+)&isA.+","\\1",rurl)
s_rurl=c()
for(j in 1:length(s_url)){
  s_rurl[j]=gsub(id,s_id[j],rurl)
  #Seller review page's url
}
ref_db=cbind(ref_db,Rev_URL=s_rurl)

l=1
r_id=c()
r_rat=c()
r_rev=c()
r_rd=c()

s_rat=as.numeric(s_rat)

for(n in 1:length(s_url)){
  print(n)
  if(!is.na(s_rat[n])){
    
    {
      p=ifelse(s_rat[n]>100,15,as.integer(s_rat[n]/8)+1)
      for(m in 0:(p-1)){
        page=substr(s_rurl[n],1,nchar(s_rurl[n])-1)
        page=sprintf("%s%d",page,m)
        b=read_html(page) %>%
          html_nodes(".feedback-num") %>%
          html_text()
        c=read_html(page) %>%
          html_nodes(".feedback-comment") %>%
          html_text()
        d=read_html(page) %>%
          html_nodes(".feedback-rater-date") %>%
          html_text()
        
        if(length(b)>0){
          for(j in 1:length(b)){
            r_id[l]=s_id[n]
            r_rat[l]=b[j]
            r_rev[l]=c[j]
            r_rd[l]=d[j]
            l=l+1
            print(b[j])
          }
        }
      }
    }
  }
}

rev_db=data.frame(Seller_ID=r_id,Rating=r_rat,Review=r_rev,Reviewer_Date=r_rd)

url="http://www.amazon.in/s?marketplaceID=A21TJRUUN4KGV&me=ADRSR1T7JUIYC&merchant=ADRSR1T7JUIYC&redirect=true"
id=sub(".+&me=(.+)&m.+","\\1",url)

s_products=c()
s_about=c()
n_url=c()
l=1

for(i in 1:length(s_url))
{
  print(i)
  Next_Page = tryCatch(read_html(s_url[i])%>%
                         html_node(".aag_storefront a")%>%
                         html_attr("href"),error=function(e) NA)
  
  n_url[i]=gsub(id,s_id[i],url)
  
  
  Items_Info=NA
  while(is.na(Items_Info)){
    if(s_url[i]=="http://www.amazon.in"){break}
    Items_Info = tryCatch(read_html(n_url[i])%>% html_node(".s-first-column h2")%>% html_text(),error=function(e) NA)
    # number of products sold by this seller
  }
  read_html(s_url[i])
  Sys.sleep(1)
  Next_Page2=tryCatch(read_html(s_url[i]) %>% html_node(".aag_hdr_legalInfo a")%>%html_attr("href"),error=function(e) NA)
  Next_Page2=paste0("http://www.amazon.in",Next_Page2)
  aboutseller=tryCatch(read_html(Next_Page2)%>%html_nodes("#aag_detailsAbout p")%>%html_text(),error=function(e) NA)
  # about the seller
  print(Items_Info)
  s_products[l]=Items_Info
  s_about[l]=aboutseller
  l=l+1
}
feed=data.frame(Seller_ID=s_id,No_Of_Products=s_products,About_Seller=s_about)
