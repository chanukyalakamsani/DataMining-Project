from django.http import HttpResponseRedirect
from django.shortcuts import render
from django.views.generic import TemplateView
from .forms import NameForm
import pandas as pd
from .youtubeexplorer import youtube_search


class Home(TemplateView):
    template_name = 'upload.html'



def get_name(request):
    # if this is a POST request we need to process the form data
    if request.method == 'POST':
        # create a form instance and populate it with data from the request:
        form = NameForm(request.POST)
        # check whether it's valid:
        if form.is_valid():
            subject = form.cleaned_data['your_name']
            test = youtube_search(subject)
            df = pd.DataFrame(data=test)
            print(df['title'])
            form = {}
            # process the data in form.cleaned_data as required
            # ...
            # redirect to a new URL:


    # if a GET (or any other method) we'll create a blank form
    else:
        form = NameForm()

    return render(request, 'upload.html', {'form': form})


# Create your views here.
