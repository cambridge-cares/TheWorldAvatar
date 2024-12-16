import { Link } from 'react-router-dom';

function GuidanceApp()  {
return <div>
            <nav class="navbar navbar-dark">
                <div class="row mx-1">
                    <a class="col-1 "  href="https://dev.theworldavatar.io/">
                        <img alt="Alt content" src={require('./../assets/images/logo_sm.png')} />
                </a>
                <h2 className="navbar-brand mx-3 col-8">Powered by The World Avatar
                    </h2>
                </div>
            </nav>
            <div className="container-fluid content-container col-12 col-md-9 col-xl-8 py-md-3 pl-md-5 regular-bd" role="main">
                <h3 class="bd-title">Guidance</h3>
                <h1 className="mb-5">Singapore MacKay Carbon Calculator</h1>
                 <h3>
                 The Singapore MacKay Carbon Calculator provides a model of the Singapore energy system that allows you to explore pathways to decarbonisation.
                 </h3>
               <hr/>

               <h6>Contents</h6>
               <ul>
                 <li><a href="#secMackay">The Singapore MacKay Carbon Calculator</a></li>
                 <li><a href="#secProject">The project</a></li>
                 <li><a href="#secDisclaimer">Terms and conditions</a></li>
                      <li><a href="#secContact">Contact us</a></li>

               </ul>
             <p>
                The Singapore MacKay Carbon Calculator is inspired by the UK MacKay Carbon Calculator, that will allow Singapore to explore pathways to decarbonization. The Singapore MacKay Carbon Calculator will closely resemble the UK version in terms of its user interface and underlying modeling approaches.</p>
               <p> The calculator is named in honour of the late Sir David MacKay who was the driving force behind the first UK 2050 calculator.

                </p>

                <h2 id='secMackay'>The Singapore MacKay Carbon Calculator</h2>
                <h4> What is a MacKay Carbon Calculator?</h4>
                <p> The MacKay Carbon Calculator, named after the late Professor Sir David MacKay, was originally developed for the UK as part of its efforts to address climate change challenges.</p>
                 <p>This innovative tool enables users to explore different energy and emissions scenarios, helping to explore how various choices impact a nation's carbon footprint. The success of the UK model has inspired several countries to adopt and adapt the MacKay Carbon Calculator to their specific national contexts. These adaptations have provided valuable insights into national energy strategies, assisting in policy formation geared towards sustainable energy use and carbon reduction.</p>
                 <h4>What is the Singapore-MacKay Carbon Calculator?</h4>
                 <p> Our Singapore-MacKay Carbon Calculator, while closely mirroring the UK version in terms of underlying modelling approaches and assumptions, has been specifically adapted to reflect Singapore’s unique context through the update of various input variables. These modifications include demographic factors such as population and housing projections, local environmental conditions like external ambient temperature, and sector-specific elements such as fuel consumption and CO2 emissions profiles. Additionally, adjustments account for Singapore's specific characteristics in areas like travel modes, domestic forestry, farming, waste management, and electricity generation.</p>
                <p> On the other hand, certain aspects of the calculator remain unchanged, consistent with the original UK model. These include emission factors, efficiencies of technologies and processes, energy intensities, and scrappage decisions, among others.</p>
                 <p>As part of our research and as a proof-of-concept, the Singapore-MacKay Carbon Calculator is augmented with The World Avatar to better incorporate selected data from diverse sources, thereby improving its modelling capabilities.
                 Where does the data come from? </p>
                 <p> Data for these modifications are sourced from various sources, including the Singapore Department of Statistics, National Communications Reports, and other relevant government and industry data sources.  </p>
                 <h4> What are its limitations? </h4>
                 <p> Our Singapore-MacKay Carbon Calculator inherits certain assumptions and priorities from the UK model, such as levels of ambition and the base year definition. Discrepancies in data availability for specific years and assumptions about fuel types also introduce variations in CO2 emission estimations. Furthermore, the model does not yet incorporate cost considerations and has specific approaches to accounting for emissions from the sale of international aviation and marine bunker fuels, reflecting Singapore's unique position as a major transportation hub. These limitations highlight areas where the model may deviate from Singapore's specific realities and indicate potential avenues for further refinement. </p>
                 <h4>  How do I use it? </h4>
                <p> The calculator is designed to be intuitive and user-friendly, allowing the general public to engage with complex energy and climate-related issues in an accessible way. Each sector comprises adjustable parameters related to energy consumption and carbon emissions. To modify a parameter, use the sliders for the input fields provided. As you adjust parameters, experiment with different scenarios to see how changes in one sector affect overall emissions. Observe the feedback on emissions projections and energy mix displayed by the calculator.</p>



                <h2 id="secProject">The project</h2>
                The Singapore MacKay Carbon Calculator is part of the <a href='https://dev.theworldavatar.io/'>World Avatar</a> and a work of the <a href='https://www.cares.cam.ac.uk'>Cambridge CARES</a>.

                <h2 id="secDisclaimer">Terms and conditions</h2>
This page and any pages it links to explains our content’s terms of use. You must agree to these to use our content.


                 <h4>Using our content</h4>
<p>You agree to use our content only for lawful purposes. You must also use it in a way that does not infringe the rights of, or restrict or inhibit the use and enjoyment of, this site by anyone else.</p>

<p>We update our content all the time. We can change or remove content at any time without notice. </p>

<p>We do not give any guarantees, conditions or warranties about the accuracy or completeness of any content used by these products. We’re not liable for any loss or damage that may come from your use of these products.</p>

<p>The most up to date version of our content will always be on the Singapore Mackay Carbon Calculator website.</p>

 <h4>Linking to the Singapore Mackay Carbon Calculator</h4>
<p>We welcome and encourage other websites to link to the Singapore Mackay Carbon Calculator.</p>
<p>You must  <a href="https://www.cares.cam.ac.uk/contact-us/">contact us</a> for permission if you want to either:</p>
<ul>
<li>say your website is associated with or endorsed by Cambridge CARES</li>
</ul>


 <h4>Linking from the Singapore Mackay Carbon Calculator</h4>
<p>The Singapore Mackay Carbon Calculator may link to websites that are managed by other agencies, service providers or organisations. We do not have any control over the content on these websites.</p>

<p>We’re not responsible for:</p>
<ul>
<li>the protection of any information you give to these websites</li>
<li>any loss or damage that may come from your use of these websites, or any other websites they link to</li>
<li>You agree to release us from any claims or disputes that may come from using these websites.</li>
</ul>
<p>You should read all terms and conditions, privacy policies and end user licences that relate to these websites before you use them.</p>

            <h4>Requests to remove content</h4>

<p>You can ask for content to be removed from our website. We’ll remove content:</p>
<ul>
<li>in order to comply with data protection legislation covering the rights and freedoms of individuals</li>
<li>if it breaches copyright laws, contains sensitive personal data or material that may be considered obscene or defamatory</li>
</ul>

<p> <a href="https://www.cares.cam.ac.uk/contact-us/">Contact us</a> to ask for content to be removed. You’ll need to send us the web address (URL) of the content and explain why you think it should be removed. We’ll reply to let you know whether we’ll remove it.</p>



<h4>Virus protection</h4>
<p>You must make sure that the way you use any of our websites does not expose you to the risk of viruses, malicious computer code or other forms of interference which can damage your computer system.</p>

<p>We’re not responsible for any loss, disruption or damage to your data or computer system that might happen when you use any of our websites.</p>


<h4>Viruses, hacking and other offences</h4>
<p>When using any of our websites, you must not introduce viruses, trojans, worms, logic bombs or any other material that’s malicious or technologically harmful.</p>

<p>You must not try to gain unauthorised access to any of our websites, the server on which it’s stored or any server, computer or database connected to it.</p>

<p>You must not attack any of our websites in any way. This includes denial-of-service attacks.</p>

<p>We’ll report any attacks or attempts to gain unauthorised access to any of our websites to the relevant law enforcement authorities and share information about you with them.</p>


<h4>General</h4>
<p>There may be legal notices elsewhere on our website that relate to how you use the site.</p>

<p>We’re not liable if we fail to comply with these terms and conditions because of circumstances beyond our reasonable control.</p>

<p>We might decide not to exercise or enforce any right available to us under these terms and conditions. We can always decide to exercise or enforce that right at a later date.</p>

<p>Doing this once will not mean we automatically waive the right on any other occasion.</p>

<p>If any of these terms and conditions are held to be invalid, unenforceable or illegal for any reason, the remaining terms and conditions will still apply.</p>

<h4>Changes to these terms and conditions</h4>
<p>Please check these terms and conditions regularly. We can update them at any time without notice.</p>

<p>You’ll agree to any changes if you continue to use our website after the terms and conditions have been updated.</p>



         <h4>Disclaimer</h4>
<p>While we make every effort to keep our content up to date, we do not provide any guarantees, conditions or warranties that the information will be:</p>

<ul>
  <li>current</li>
  <li>secure</li>
  <li>accurate</li>
  <li>complete</li>
  <li>free from bugs or viruses</li>
</ul>

<p>We do not publish advice on our content. You should get professional or specialist advice before doing anything on the basis of the content.</p>

<p>We’re not liable for any loss or damage that may come from using our content. This includes:</p>

<ul>
  <li>any direct, indirect or consequential losses</li>
  <li>any loss or damage caused by civil wrongs (‘tort’, including negligence), breach of contract or otherwise</li>
  <li>the use of our content and any websites that are linked to or from it</li>
  <li>the inability to use our content and any websites that are linked to or from it</li>
</ul>

<p>This applies if the loss or damage was foreseeable, arose in the normal course of things or you advised us that it might happen.</p>

<p>This includes (but is not limited to) the loss of your:</p>

<ul>
  <li>income or revenue</li>
  <li>salary, benefits or other payments</li>
  <li>business</li>
  <li>profits or contracts</li>
  <li>opportunity</li>
  <li>anticipated savings</li>
  <li>data</li>
  <li>goodwill or reputation</li>
  <li>tangible property</li>
  <li>intangible property, including loss, corruption or damage to data or any computer system</li>
  <li>wasted management or office time</li>
</ul>

<p>We may still be liable for:</p>

<ul>
  <li>death or personal injury arising from our negligence</li>
  <li>fraudulent misrepresentation</li>
  <li>any other liability which cannot be excluded or limited under applicable law</li>
</ul>





                <h2 id="secContact">Contact us</h2>

Please use the following <a href="https://www.cares.cam.ac.uk/contact-us/">link</a> to get in touch with us regarding questions about The Singapore MacKay Carbon Calculator, collaboration opportunities, potential partnerships, or private consulting.

<h6>Last updated 21 November 2023</h6>

            </div>
        </div>

}

export default GuidanceApp;